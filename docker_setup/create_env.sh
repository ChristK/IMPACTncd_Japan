#!/bin/bash
# -----------------------------------------------------------------------------
# create_env.sh
#
# Usage:
#   ./create_env.sh [path_to_yaml] [--use-volumes]
#
# Description:
#   This script builds and runs a Docker container for the IMPACTncd Japan project.
#   It rebuilds the Docker image only if build inputs have changed.
#
#   When the --use-volumes flag is provided:
#     1. The entire project directory (one level above docker_setup) is copied 
#        into a Docker volume (VOLUME_PROJECT). This volume is used as the main drive 
#        during simulation.
#     2. Separate Docker volumes for 'output_dir' (VOLUME_OUTPUT_NAME) and 
#        'synthpop_dir' (VOLUME_SYNTHPOP_NAME) are created and pre-populated from
#        the local host folders.
#     3. The container runs using these volumes (project, outputs, and synthpop) for 
#        enhanced performance.
#     4. Upon container exit, the contents of the output and synthpop volumes are 
#        synchronized (copied) back to the local folders using rsync.
#     5. Finally, all the created volumes are removed.
#
#   When not using volumes, the script uses direct bind mounts. This is less
#   efficient, particularly on macOS, but allows for interactive access of the
#   model, outputs, and synthpop directories.
#
# Compatible with Linux and macOS (requires coreutils on macOS).
# -----------------------------------------------------------------------------

# Get the directory where the script is located
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
# Get the project root directory (one level above the script directory)
PROJECT_ROOT=$(realpath "$SCRIPT_DIR/..")

# Variable definitions
IMAGE_NAME="impactncd-japan-r-prerequisite:latest"
DOCKERFILE="Dockerfile.prerequisite"
HASH_FILE="$SCRIPT_DIR/.docker_build_hash" # Store hash file in script directory
YAML_FILE="$PROJECT_ROOT/inputs/sim_design.yaml" # Default YAML path relative to project root
CURRENT_USER=$(whoami)
# User-specific Docker volume names to avoid conflicts
VOLUME_PROJECT="impactncd_japan_project_${CURRENT_USER}"
VOLUME_OUTPUT_NAME="impactncd_japan_output_${CURRENT_USER}"
VOLUME_SYNTHPOP_NAME="impactncd_japan_synthpop_${CURRENT_USER}"

# --- Docker Permission Check ---
# Check if the user can connect to the Docker daemon
if ! docker info > /dev/null 2>&1; then
  echo "---------------------------------------------------------------------"
  echo "Error: Cannot connect to the Docker daemon."
  echo "Please ensure Docker is running and you have the necessary permissions."
  echo "You might need to:"
  echo "  1. Start the Docker daemon."
  echo "  2. Add your user to the 'docker' group:"
  echo "     sudo usermod -aG docker $USER"
  echo "     (You'll need to log out and back in for this change to take effect)"
  echo "  3. Or run this script using 'sudo':"
  echo "     sudo ./create_env.sh [options]"
  echo "---------------------------------------------------------------------"
  exit 1
fi
# --- End Docker Permission Check ---

# Remove stopped containers to avoid conflicts before volume operations
echo "Removing stopped containers..."
docker container prune -f

# Process command-line arguments for YAML file and volume usage flag
USE_VOLUMES=false # Default to not using volumes
for arg in "$@"; do
  if [[ "$arg" == --use-volumes ]]; then
    USE_VOLUMES=true
  elif [[ "$arg" == *.yaml || "$arg" == *.yml ]]; then
    # If YAML path is relative, resolve it relative to the current execution dir
    if [[ "$arg" != /* && "$arg" != ~* ]]; then
        YAML_FILE="$(realpath "$arg")"
    else
        YAML_FILE="$arg"
    fi
  fi
done

# Default to not using volumes if flag is not provided
USE_VOLUMES=false

# Check again for --use-volumes flag
for arg in "$@"; do
  if [[ "$arg" == "--use-volumes" ]]; then
    USE_VOLUMES=true
  fi
done

if [ ! -f "$YAML_FILE" ]; then
  echo "Error: YAML file not found at $YAML_FILE"
  exit 1
fi

echo "Using configuration from: $YAML_FILE"

# Set simulation design file and extract output directories from YAML
SIM_DESIGN_FILE="$YAML_FILE"
OUTPUT_DIR_RAW=$(grep '^output_dir:' "$SIM_DESIGN_FILE" | sed -E 's/output_dir:[[:space:]]*([^#]*).*/\1/' | xargs)
SYNTHPOP_DIR_RAW=$(grep '^synthpop_dir:' "$SIM_DESIGN_FILE" | sed -E 's/synthpop_dir:[[:space:]]*([^#]*).*/\1/' | xargs)

# Resolve paths relative to the PROJECT_ROOT if they are not absolute
if [[ "$OUTPUT_DIR_RAW" != /* && "$OUTPUT_DIR_RAW" != ~* ]]; then
  OUTPUT_DIR="$(realpath "$PROJECT_ROOT/$OUTPUT_DIR_RAW")"
else
  OUTPUT_DIR="$(realpath "$OUTPUT_DIR_RAW")" # Resolve even absolute paths to clean them (e.g. remove ..)
fi
if [[ "$SYNTHPOP_DIR_RAW" != /* && "$SYNTHPOP_DIR_RAW" != ~* ]]; then
  SYNTHPOP_DIR="$(realpath "$PROJECT_ROOT/$SYNTHPOP_DIR_RAW")"
else
  SYNTHPOP_DIR="$(realpath "$SYNTHPOP_DIR_RAW")" # Resolve even absolute paths
fi

# Validate resolved paths
if [ ! -d "$OUTPUT_DIR" ]; then
    echo "Warning: Resolved output_dir does not exist: $OUTPUT_DIR. Creating it now."
    mkdir -p "$OUTPUT_DIR"
    # exit 1 # Decide if you want to exit or create
fi
if [ ! -d "$SYNTHPOP_DIR" ]; then
    echo "Warning: Resolved synthpop_dir does not exist: $SYNTHPOP_DIR. Creating it now."
    mkdir -p "$SYNTHPOP_DIR"
    # exit 1 # Decide if you want to exit or create
fi

echo "Mounting output_dir: $OUTPUT_DIR"
echo "Mounting synthpop_dir: $SYNTHPOP_DIR"

# Detect OS and choose appropriate hash command
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
  HASH_CMD="sha256sum"
elif [[ "$OSTYPE" == "darwin"* ]]; then
  if command -v gsha256sum > /dev/null; then
    HASH_CMD="gsha256sum"
  else
    echo "Error: gsha256sum not found. Please install coreutils with:"
    echo "  brew install coreutils"
    exit 1
  fi
else
  echo "Unsupported OS: $OSTYPE"
  exit 1
fi

# Compute hash of build inputs (Dockerfile, apt-packages.txt, r-packages.txt)
BUILD_HASH=$(cat "$DOCKERFILE" apt-packages.txt r-packages.txt | $HASH_CMD | cut -d ' ' -f1)

# Determine whether rebuild of the Docker image is needed
NEEDS_BUILD=false
if ! docker image inspect "$IMAGE_NAME" > /dev/null 2>&1; then
  echo "Docker image does not exist. Need to build."
  NEEDS_BUILD=true
elif [ ! -f "$HASH_FILE" ]; then
  echo "No previous build hash found. Need to build."
  NEEDS_BUILD=true
else
  LAST_HASH=$(cat "$HASH_FILE")
  if [ "$BUILD_HASH" != "$LAST_HASH" ]; then
    echo "Detected changes in build inputs. Rebuilding Docker image..."
    NEEDS_BUILD=true
  else
    echo "No changes detected. Skipping Docker build."
  fi
fi

# Build Docker image if needed
if [ "$NEEDS_BUILD" = true ]; then
  docker build --no-cache -f "$DOCKERFILE" -t "$IMAGE_NAME" .
  echo "$BUILD_HASH" > "$HASH_FILE"
fi

# -----------------------------------------------------------------------------
# Optionally create and use Docker volumes for simulation
#
# When using volumes:
#
#   - The entire project directory (parent of docker_setup) is copied to a Docker
#     volume (VOLUME_PROJECT). This volume acts as the main drive during simulation.
#
#   - Separate volumes (VOLUME_OUTPUT_NAME and VOLUME_SYNTHPOP_NAME) for the outputs 
#     and synthpop directories (as specified in the YAML file) are created.
#
#   - Prior to simulation, the local outputs and synthpop folders are copied into these volumes.
#
#   - The container runs with these Docker volumes mounted. This improves I/O performance.
#
#   - After the container exits, the content of the output and synthpop volumes is 
#     synchronized back to the corresponding local folders using rsync.
#
#   - Finally, all these Docker volumes are removed to clean up.
#
# When not using volumes, the script uses direct bind mounts.
# -----------------------------------------------------------------------------
if [ "$USE_VOLUMES" = true ]; then
  echo "Using Docker volumes for project, outputs, and synthpop..."

  # Build rsync-alpine image (only if it doesn't already exist)
  if ! docker image inspect rsync-alpine > /dev/null 2>&1; then
    echo "Building rsync-alpine image..."
    docker build -f Dockerfile.rsync -t rsync-alpine .
  else
    echo "Using existing rsync-alpine image."
  fi

  # Ensure local output directories exist
  mkdir -p "$OUTPUT_DIR"
  mkdir -p "$SYNTHPOP_DIR"

  # Prune stopped containers to free volume locks
  echo "Pruning stopped containers..."
  docker container prune -f

  # Remove any existing volumes (ignore errors if not removable)
  echo "Removing any existing volumes (if possible)..."
  docker volume rm "$VOLUME_PROJECT" 2>/dev/null
  docker volume rm "$VOLUME_OUTPUT_NAME" 2>/dev/null
  docker volume rm "$VOLUME_SYNTHPOP_NAME" 2>/dev/null

  # Create fresh Docker-managed volumes
  docker volume create "$VOLUME_PROJECT"
  docker volume create "$VOLUME_OUTPUT_NAME"
  docker volume create "$VOLUME_SYNTHPOP_NAME"

  # --------------------------------------------------------------------------
  # Pre-populate volumes:
  #
  # 1. The project volume is populated with the entire project directory (from
  #    one level above the docker_setup folder), excluding dot files/folders.
  #    This volume serves as the main drive.
  #
  # 2. The output and synthpop volumes are populated from the respective local folders.
  # --------------------------------------------------------------------------
  echo "Populating project volume from host project directory (excluding dot files/folders)..."
  # Use tar to copy, excluding dot files/folders at the root of the source
  docker run --rm \
    -v "${PROJECT_ROOT}":/source \
    -v "${VOLUME_PROJECT}":/destination \
    alpine sh -c "tar -C /source --exclude='./.*' -cf - . | tar -C /destination -xf -"

  echo "Populating output and synthpop volumes from local folders..."
  docker run --rm \
    -v "$OUTPUT_DIR":/source \
    -v "$VOLUME_OUTPUT_NAME":/volume \
    alpine sh -c "cp -a /source/. /volume/"
  docker run --rm \
    -v "$SYNTHPOP_DIR":/source \
    -v "$VOLUME_SYNTHPOP_NAME":/volume \
    alpine sh -c "cp -a /source/. /volume/"

  # Run the main container with the project volume mounted in place of the project bind mount.
  echo "Running the main container using Docker volumes..."
  docker run -it \
    --mount type=volume,source="$VOLUME_PROJECT",target=/IMPACTncd_Japan \
    --mount type=volume,source="$VOLUME_OUTPUT_NAME",target=/IMPACTncd_Japan/output \
    --mount type=volume,source="$VOLUME_SYNTHPOP_NAME",target=/IMPACTncd_Japan/synthpop \
    --workdir /IMPACTncd_Japan \
    "$IMAGE_NAME" \
    bash

  # After the container exits:
  # - Synchronize the volumes back to the local directories using rsync (checksum mode).
  echo "Container exited. Syncing volumes back to local directories using rsync (checksum mode)..."
  docker run --rm \
    -v "$VOLUME_OUTPUT_NAME":/volume \
    -v "$OUTPUT_DIR":/backup \
    rsync-alpine rsync -avc /volume/ /backup/
  docker run --rm \
    -v "$VOLUME_SYNTHPOP_NAME":/volume \
    -v "$SYNTHPOP_DIR":/backup \
    rsync-alpine rsync -avc /volume/ /backup/

  # Clean up all the Docker volumes used for the simulation.
  echo "Cleaning up Docker volumes..."
  docker container prune -f
  docker volume rm "$VOLUME_PROJECT"
  docker volume rm "$VOLUME_OUTPUT_NAME"
  docker volume rm "$VOLUME_SYNTHPOP_NAME"
else
  echo "Using direct bind mounts for project, outputs, and synthpop..."

  docker run -it \
    --mount type=bind,source="$PROJECT_ROOT",target=/IMPACTncd_Japan \
    --mount type=bind,source="$OUTPUT_DIR",target=/IMPACTncd_Japan/output \
    --mount type=bind,source="$SYNTHPOP_DIR",target=/IMPACTncd_Japan/synthpop \
    --workdir /IMPACTncd_Japan \
    "$IMAGE_NAME" \
    bash
fi
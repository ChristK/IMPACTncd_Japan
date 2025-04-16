# -----------------------------------------------------------------------------
# create_env.ps1
#
# PowerShell script for building and running a Docker container for the
# IMPACTncd Japan project. This version supports two operation modes:
#
# 1. Using Docker-managed volumes for enhanced I/O performance (recommended
#    for macOS and Windows). In this mode:
#      - The entire project directory (one level above docker_setup) is copied
#        into a Docker volume (project volume) that becomes the main drive during
#        simulation.
#      - Separate Docker volumes for the output_dir and synthpop_dir (as defined
#        in the YAML file) are created and pre-populated from the local folders.
#      - After the container exits, the contents of the output and synthpop volumes
#        are synchronized back to the corresponding local folders using rsync.
#      - All volumes are then removed.
#
# 2. Using direct bind mounts (less efficient, but useful for interactive access).
#
# Usage:
#   .\create_env.ps1 [-SimDesignYaml <path\to\sim_design.yaml>] [-UseVolumes]
#
# If you get an execution policy error, run:
#   Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass
#
# -----------------------------------------------------------------------------

param (
    [string]$SimDesignYaml = "..\inputs\sim_design.yaml",
    [switch]$UseVolumes
)

# Resolve script directory and switch to it
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
Push-Location $ScriptDir

# Validate that the YAML file exists
if (-not (Test-Path $SimDesignYaml)) {
    Write-Host "Error: YAML file not found at '$SimDesignYaml'"
    Pop-Location
    Exit 1
}

Write-Host "Using configuration file: $SimDesignYaml"

# Variable definitions
$ImageName   = "impactncd-japan-r-prerequisite:latest"
$Dockerfile  = "Dockerfile.prerequisite"
$HashFile    = ".docker_build_hash"

# Get current user (for user-specific volume names)
$CurrentUser = $env:USERNAME

# Define user-specific Docker volume names
$VolumeProject   = "impactncd_japan_project_$CurrentUser"
$VolumeOutput    = "impactncd_japan_output_$CurrentUser"
$VolumeSynthpop  = "impactncd_japan_synthpop_$CurrentUser"

# -----------------------------
# Build hash and rebuild logic
# -----------------------------
# Helper function to normalize file contents
function Get-NormalizedContent {
    param([string]$Path)
    $content = Get-Content -Raw -Encoding UTF8 $Path
    return ($content -replace "`r`n", "`n").TrimEnd()
}

# Compute robust build hash from Dockerfile and package lists
$FilesToHash = @(
    Get-NormalizedContent -Path $Dockerfile
    Get-NormalizedContent -Path "apt-packages.txt"
    Get-NormalizedContent -Path "r-packages.txt"
)
$JoinedContent = ($FilesToHash -join "`n")
$Bytes = [System.Text.Encoding]::UTF8.GetBytes($JoinedContent)
$HashAlgorithm = [System.Security.Cryptography.SHA256]::Create()
$HashBytes = $HashAlgorithm.ComputeHash($Bytes)
$BuildHash = ([BitConverter]::ToString($HashBytes) -replace "-", "")

$NeedsBuild = $false
docker image inspect $ImageName > $null 2>&1
if ($LASTEXITCODE -ne 0) {
    Write-Host "Docker image does not exist. Need to build."
    $NeedsBuild = $true
} elseif (-not (Test-Path $HashFile)) {
    Write-Host "No previous build hash found. Need to build."
    $NeedsBuild = $true
} else {
    $LastHash = (Get-Content -Raw -Encoding UTF8 $HashFile).Trim()
    if ($LastHash -ne $BuildHash) {
        Write-Host "Detected changes in build inputs. Rebuilding Docker image..."
        $NeedsBuild = $true
    } else {
        Write-Host "No changes detected. Skipping Docker build."
    }
}

# Build Docker image if needed
if ($NeedsBuild) {
    Write-Host "Building Docker image using --no-cache..."
    docker build --no-cache -f $Dockerfile -t $ImageName .
    $BuildHash | Set-Content -NoNewline -Encoding UTF8 $HashFile
} else {
    Write-Host "Docker image is up to date. Skipping build."
}

# -----------------------------
# Extract paths from YAML
# -----------------------------
function Get-YamlPathValue {
    param (
        [string]$YamlPath,
        [string]$Key
    )
    $line = Select-String -Path $YamlPath -Pattern "^$Key\s*:" | Select-Object -First 1
    if ($line) {
        $value = ($line.Line -split ":\s*", 2)[1].Split("#")[0].Trim()
        try {
            $resolved = Resolve-Path $value -ErrorAction Stop
            # Normalize to forward slashes for Docker
            $normalized = $resolved.Path -replace '\\', '/'
            return $normalized
        } catch {
            Write-Host "Could not resolve path: $value"
            return $null
        }
    }
    Write-Host "Warning: No matching line found for key: $Key"
    return $null
}

$outputDir    = Get-YamlPathValue -YamlPath $SimDesignYaml -Key "output_dir"
$synthpopDir  = Get-YamlPathValue -YamlPath $SimDesignYaml -Key "synthpop_dir"

if (-not $outputDir -or -not (Test-Path $outputDir)) {
    Write-Host "Error: output_dir path not found or invalid: $outputDir"
    Pop-Location
    Exit 1
}
if (-not $synthpopDir -or -not (Test-Path $synthpopDir)) {
    Write-Host "Error: synthpop_dir path not found or invalid: $synthpopDir"
    Pop-Location
    Exit 1
}

Write-Host "Mounting output_dir:    $outputDir"
Write-Host "Mounting synthpop_dir:  $synthpopDir"

# Resolve project root directory (one level above the current script directory)
$ProjectRoot = (Resolve-Path "$ScriptDir/..").Path -replace '\\', '/'

# -----------------------------
# Run Docker container
# -----------------------------
if ($UseVolumes) {
    Write-Host "`nUsing Docker volumes for project, outputs, and synthpop..."

    # Build rsync-alpine image if it doesn't already exist.
    $rsyncImage = "rsync-alpine"
    docker image inspect $rsyncImage > $null 2>&1
    if ($LASTEXITCODE -ne 0) {
        Write-Host "Building rsync-alpine image..."
        docker build -f "Dockerfile.rsync" -t $rsyncImage .
    } else {
        Write-Host "Using existing rsync-alpine image."
    }

    # Ensure local output directories exist
    if (-not (Test-Path $outputDir)) { New-Item -ItemType Directory -Path $outputDir | Out-Null }
    if (-not (Test-Path $synthpopDir)) { New-Item -ItemType Directory -Path $synthpopDir | Out-Null }

    # Clean up stopped containers to free volume locks
    Write-Host "Pruning stopped containers..."
    docker container prune -f | Out-Null

    # Remove any existing volumes (ignore errors if not removable)
    Write-Host "Removing any existing volumes (if possible)..."
    docker volume rm $VolumeProject -f 2>$null
    docker volume rm $VolumeOutput -f 2>$null
    docker volume rm $VolumeSynthpop -f 2>$null

    # Create fresh Docker-managed volumes
    docker volume create $VolumeProject | Out-Null
    docker volume create $VolumeOutput | Out-Null
    docker volume create $VolumeSynthpop | Out-Null

    # Pre-populate volumes:
    #
    # 1. The project volume is populated with the entire project directory.
    # 2. The output and synthpop volumes are populated from the respective local folders.
    Write-Host "Populating project volume from host project directory..."
    docker run --rm -v "$ProjectRoot":/source -v $VolumeProject:/destination alpine sh -c "cp -a /source/. /destination/"

    Write-Host "Populating output volume from local folder..."
    docker run --rm -v "$outputDir":/source -v $VolumeOutput:/volume alpine sh -c "cp -a /source/. /volume/"
    Write-Host "Populating synthpop volume from local folder..."
    docker run --rm -v "$synthpopDir":/source -v $VolumeSynthpop:/volume alpine sh -c "cp -a /source/. /volume/"

    # Run the main container with volumes mounted.
    Write-Host "Running the main container using Docker volumes..."
    docker run -it `
        --mount type=volume,source=$VolumeProject,target=/IMPACTncd_Japan `
        --mount type=volume,source=$VolumeOutput,target=/IMPACTncd_Japan/output `
        --mount type=volume,source=$VolumeSynthpop,target=/IMPACTncd_Japan/synthpop `
        --workdir /IMPACTncd_Japan `
        $ImageName `
        bash

    # After the container exits:
    # Synchronize the output and synthpop volumes back to the local directories using rsync.
    Write-Host "Container exited. Syncing volumes back to local directories using rsync (checksum mode)..."
    docker run --rm -v $VolumeOutput:/volume -v "$outputDir":/backup $rsyncImage rsync -avc /volume/ /backup/
    docker run --rm -v $VolumeSynthpop:/volume -v "$synthpopDir":/backup $rsyncImage rsync -avc /volume/ /backup/

    # Clean up all the Docker volumes used for the simulation.
    Write-Host "Cleaning up Docker volumes..."
    docker container prune -f | Out-Null
    docker volume rm $VolumeProject | Out-Null
    docker volume rm $VolumeOutput | Out-Null
    docker volume rm $VolumeSynthpop | Out-Null

} else {
    Write-Host "`nUsing direct bind mounts for project, outputs, and synthpop..."
    docker run -it `
        --mount type=bind,source="$ProjectRoot",target=/IMPACTncd_Japan `
        --mount type=bind,source="$outputDir",target=/IMPACTncd_Japan/output `
        --mount type=bind,source="$synthpopDir",target=/IMPACTncd_Japan/synthpop `
        --workdir /IMPACTncd_Japan `
        $ImageName `
        bash
}

# Restore the original directory
Pop-Location
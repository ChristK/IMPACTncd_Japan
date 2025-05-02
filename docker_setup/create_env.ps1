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

# Resolve project root directory (one level above the current script directory)
$ProjectRoot = (Resolve-Path "$ScriptDir/..").Path -replace '\\', '/'

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

# --- Docker Permission Check ---
# Check if the user can connect to the Docker daemon
Write-Host "Checking Docker daemon connectivity..."
docker info > $null 2>&1
if ($LASTEXITCODE -ne 0) {
    Write-Host "---------------------------------------------------------------------" -ForegroundColor Red
    Write-Host "Error: Cannot connect to the Docker daemon." -ForegroundColor Red
    Write-Host "This usually means Docker Desktop (Windows/macOS) or the Docker service (Linux) is not running or your user account lacks permission."
    Write-Host "Please ensure Docker is running and accessible before proceeding."
    Write-Host "" # Blank line for spacing
    Write-Host "How to check/fix:" -ForegroundColor Yellow
    Write-Host "  1. Run 'docker info' in your terminal. If it fails with a similar error, Docker is not accessible."
    Write-Host "  2. Windows/macOS: Make sure Docker Desktop is running (check the system tray or application list)."
    Write-Host "  3. Linux: Check service status with 'sudo systemctl status docker'. If inactive, start it with 'sudo systemctl start docker'."
    Write-Host "     You might also need to add your user to the 'docker' group ('sudo usermod -aG docker $env:USERNAME') and then log out and back in."
    Write-Host "  4. If running in WSL (Windows Subsystem for Linux), ensure Docker Desktop's WSL integration is enabled for your distribution."
    Write-Host "---------------------------------------------------------------------" -ForegroundColor Red
    Pop-Location # Restore original location before exiting
    Exit 1
} else {
    Write-Host "Docker daemon connection successful."
}
# --- End Docker Permission Check ---

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
Write-Host "Checking for Docker image: '$ImageName'"
docker image inspect $ImageName > $null 2>&1
$InspectExitCode = $LASTEXITCODE
Write-Host "docker image inspect exit code: $InspectExitCode"

if ($InspectExitCode -ne 0) {
    Write-Host "Docker image does not exist or inspect failed. Need to build."
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
# Helper function to extract and construct potential paths from the YAML file
function Get-YamlPathValue {
    param (
        [string]$YamlPath,
        [string]$Key,
        [string]$BaseDir # Pass ProjectRoot here (already uses forward slashes)
    )
    $line = Select-String -Path $YamlPath -Pattern "^$Key\s*:" | Select-Object -First 1
    if ($line) {
        $value = ($line.Line -split ":\s*", 2)[1].Split("#")[0].Trim()
        $constructedPath = $null

        # Check if the path from YAML is absolute (Windows or Unix-like)
        if ([System.IO.Path]::IsPathRooted($value) -or $value.StartsWith('/')) {
            $constructedPath = $value
            Write-Host "Path '$value' for key '$Key' is absolute."
        } else {
            # Construct path relative to the specified BaseDir (ProjectRoot)
            # Ensure BaseDir and value use consistent slashes for joining
            $valueNormalized = $value -replace '\\', '/'
            $constructedPath = "$BaseDir/$valueNormalized" # Simple string concatenation with forward slashes
            # Clean up potential double slashes, except after protocol like C://
            $constructedPath = $constructedPath -replace '(?<!:)/{2,}', '/'
            Write-Host "Path '$value' for key '$Key' is relative. Constructed as '$constructedPath'."
        }

        # Normalize to forward slashes for consistency before returning
        $normalizedPath = $constructedPath -replace '\\', '/'
        return $normalizedPath
    }
    Write-Host "Warning: No matching line found for key: $Key in '$YamlPath'"
    return $null
}

# Call the function passing $ProjectRoot
$outputDir    = Get-YamlPathValue -YamlPath $SimDesignYaml -Key "output_dir" -BaseDir $ProjectRoot
$synthpopDir  = Get-YamlPathValue -YamlPath $SimDesignYaml -Key "synthpop_dir" -BaseDir $ProjectRoot

# --- Path Validation and Creation ---
# Helper function to check and create directory
function Ensure-DirectoryExists {
    param(
        [string]$Path,
        [string]$PathKey # For logging purposes (e.g., "output_dir")
    )
    if (-not $Path) {
        Write-Host "Error: Could not determine $PathKey path from YAML."
        return $false
    }

    # Use native path format for Test-Path and New-Item
    $NativePath = $Path -replace '/', '\\'

    if (-not (Test-Path $NativePath)) {
        Write-Host "Warning: $PathKey path not found: $NativePath. Creating directory..."
        try {
            New-Item -ItemType Directory -Path $NativePath -Force -ErrorAction Stop | Out-Null
            Write-Host "Successfully created $PathKey directory: $NativePath"
            return $true
        } catch {
            Write-Host "Error: Failed to create $PathKey directory: $NativePath - $($_.Exception.Message)"
            # Attempt to resolve the path to see if it exists now, maybe a race condition or delay
            if(Test-Path $NativePath) {
                 Write-Host "Info: Directory $NativePath seems to exist now despite previous error."
                 return $true
            }
            return $false
        }
    } elseif (-not (Get-Item $NativePath).PSIsContainer) {
        Write-Host "Error: The path specified for $PathKey exists but is a file, not a directory: $NativePath"
        return $false
    } else {
         # Directory exists
         return $true
    }
}

# Validate or create output directory
if (-not (Ensure-DirectoryExists -Path $outputDir -PathKey "output_dir")) {
    Pop-Location
    Exit 1
}

# Validate or create synthpop directory
if (-not (Ensure-DirectoryExists -Path $synthpopDir -PathKey "synthpop_dir")) {
    Pop-Location
    Exit 1
}
# --- End Path Validation and Creation ---


Write-Host "Mounting output_dir:    $outputDir"       # Keep using forward slashes for Docker mounts
Write-Host "Mounting synthpop_dir:  $synthpopDir"      # Keep using forward slashes for Docker mounts

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
    # 1. The project volume is populated with the entire project directory using tar, excluding dot files/folders.
    # 2. The output and synthpop volumes are populated from the respective local folders.
    Write-Host "Populating project volume from host project directory (excluding dot files/folders)..."
    # Use tar to copy project files, excluding folders starting with .
    docker run --rm -v "${ProjectRoot}:/source" -v "${VolumeProject}:/destination" alpine sh -c "cd /source && tar cf - --exclude='./.*' . | (cd /destination && tar xf -)"

    Write-Host "Populating output volume from local folder..."
    # Use cp -Rp here as well for consistency, though less likely to cause issues
    docker run --rm -v "${outputDir}:/source" -v "${VolumeOutput}:/volume" alpine sh -c "cp -Rp /source/. /volume/"
    Write-Host "Populating synthpop volume from local folder..."
    # Use cp -Rp here as well for consistency
    docker run --rm -v "${synthpopDir}:/source" -v "${VolumeSynthpop}:/volume" alpine sh -c "cp -Rp /source/. /volume/"

    # Run the main container with volumes mounted.
    Write-Host "Running the main container using Docker volumes..."
    # Construct arguments as an array for reliable passing
    $dockerArgs = @(
        "run", "-it",
        # Use -v syntax within the array elements
        "-v", "${VolumeProject}:/IMPACTncd_Japan",
        "-v", "${VolumeOutput}:/output",
        "-v", "${VolumeSynthpop}:/synthpop",
        "--workdir", "/IMPACTncd_Japan",
        $ImageName,
        "bash"
    )
    # Execute docker with the arguments array
    & docker $dockerArgs

    # After the container exits:
    # Synchronize the output and synthpop volumes back to the local directories using rsync.
    Write-Host "Container exited. Syncing volumes back to local directories using rsync (checksum mode)..."
    # Use ${} to delimit variable name before the colon
    docker run --rm -v "${VolumeOutput}:/volume" -v "${outputDir}:/backup" $rsyncImage rsync -avc /volume/ /backup/
    docker run --rm -v "${VolumeSynthpop}:/volume" -v "${synthpopDir}:/backup" $rsyncImage rsync -avc /volume/ /backup/

    # Clean up all the Docker volumes used for the simulation.
    Write-Host "Cleaning up Docker volumes..."
    docker container prune -f | Out-Null
    docker volume rm $VolumeProject | Out-Null
    docker volume rm $VolumeOutput | Out-Null
    docker volume rm $VolumeSynthpop | Out-Null

} else {
    # Helper function to convert Windows path to Docker Desktop/WSL format
    function Convert-PathToDockerFormat {
        param([string]$Path)
        # Input example: P:/My_Models/IMPACTncd_Japan
        # Match drive letter (e.g., P) and the rest of the path
        if ($Path -match '^([A-Za-z]):/(.*)') {
            $driveLetter = $matches[1].ToLower()
            $restOfPath = $matches[2]
            # Construct the Docker path: /<drive_letter>/<rest_of_path>
            $dockerPath = "/$driveLetter/$restOfPath"
            # Remove trailing slash if present
            $dockerPath = $dockerPath -replace '/$', ''
            return $dockerPath
        } else {
            Write-Warning "Path '$Path' did not match expected Windows format (e.g., C:/path/to/dir)"
            return $Path # Return original path if format is unexpected
        }
    }

    Write-Host "`nUsing direct bind mounts for project, outputs, and synthpop..."

    # Convert paths for Docker bind mount
    $DockerProjectRoot = Convert-PathToDockerFormat -Path $ProjectRoot
    $DockerOutputDir = Convert-PathToDockerFormat -Path $outputDir
    $DockerSynthpopDir = Convert-PathToDockerFormat -Path $synthpopDir

    Write-Host "Docker Project Root: $DockerProjectRoot"
    Write-Host "Docker Output Dir:   $DockerOutputDir"
    Write-Host "Docker Synthpop Dir: $DockerSynthpopDir"

    # Pass mount arguments correctly to docker run
    docker run -it `
        --mount "type=bind,source=$DockerProjectRoot,target=/IMPACTncd_Japan" `
        --mount "type=bind,source=$DockerOutputDir,target=/output" `
        --mount "type=bind,source=$DockerSynthpopDir,target=/synthpop" `
        --workdir /IMPACTncd_Japan `
        $ImageName `
        bash
}

# Restore the original directory
Pop-Location
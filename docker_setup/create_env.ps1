# -----------------------------------------------------------------------------
# create_env.ps1
#
# PowerShell script for building and running a Docker container for the
# IMPACTncd Japan project on Windows.
# Rebuilds the Docker image only if Dockerfile.prerequisite, apt-packages.txt,
# or r-packages.txt have changed.
# This script is designed to be run from the docker_setup directory, and binds 
# its parent directory to /IMPACTncd_Japan in the container.
# If you get an execution policy error, run: 
# Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass
# -----------------------------------------------------------------------------

$ImageName = "impactncd-japan-r-prerequisite:latest"
$Dockerfile = "Dockerfile.prerequisite"
$HashFile = ".docker_build_hash"

# Resolve full paths
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
Push-Location $ScriptDir

# Read files and compute hash
$FilesToHash = @(
    Get-Content "$Dockerfile" -Raw
    Get-Content "apt-packages.txt" -Raw
    Get-Content "r-packages.txt" -Raw
)
$HashAlgorithm = [System.Security.Cryptography.SHA256]::Create()
$Bytes = [System.Text.Encoding]::UTF8.GetBytes($FilesToHash -join "`n")
$HashBytes = $HashAlgorithm.ComputeHash($Bytes)
$BuildHash = [BitConverter]::ToString($HashBytes) -replace "-", ""

# Check if image exists
$ImageExists = docker image inspect $ImageName > $null 2>&1

# Determine if rebuild is needed
$NeedsBuild = $false
if (-not $ImageExists) {
    Write-Host "Docker image does not exist. Need to build."
    $NeedsBuild = $true
} elseif (-not (Test-Path $HashFile)) {
    Write-Host "No previous build hash found. Need to build."
    $NeedsBuild = $true
} else {
    $LastHash = Get-Content $HashFile -Raw
    if ($LastHash -ne $BuildHash) {
        Write-Host "Detected changes in build inputs. Rebuilding Docker image..."
        $NeedsBuild = $true
    } else {
        Write-Host "No changes detected. Skipping Docker build."
    }
}

# Build image if needed
if ($NeedsBuild) {
    docker build --no-cache -f $Dockerfile -t $ImageName .
    $BuildHash | Set-Content $HashFile
}

# Determine parent directory to mount
$ParentDir = Resolve-Path "$ScriptDir\.."
$ParentDir = $ParentDir.Path -replace '\\', '/'

# Run Docker container
docker run -it `
  --mount type=bind,source="$ParentDir",target=/IMPACTncd_Japan `
  $ImageName `
  bash

Pop-Location
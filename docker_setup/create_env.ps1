# -----------------------------------------------------------------------------
# create_env.ps1
#
# PowerShell script for building and running a Docker container for the
# IMPACTncd Japan project on Windows.
#
# Features:
# - Accepts optional path to sim_design.yaml as argument
# - Extracts `output_dir` and `synthpop_dir` from YAML
# - Rebuilds Docker image only if build inputs have changed
# - Mounts project root, output_dir, and synthpop_dir into container
#
# Usage:
#   .\create_env.ps1 [path\to\sim_design.yaml]
#
# If you get an execution policy error, run: 
# Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass
# -----------------------------------------------------------------------------

param (
    [string]$SimDesignYaml = "..\inputs\sim_design.yaml"
)

$ImageName   = "impactncd-japan-r-prerequisite:latest"
$Dockerfile  = "Dockerfile.prerequisite"
$HashFile    = ".docker_build_hash"

# Resolve script directory
$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
Push-Location $ScriptDir

# Validate YAML path
if (-not (Test-Path $SimDesignYaml)) {
    Write-Host "Error: YAML file not found at '$SimDesignYaml'"
    Exit 1
}

Write-Host "Using configuration file: $SimDesignYaml"

# Helper function to normalize file contents
function Get-NormalizedContent {
    param([string]$Path)
    $content = Get-Content -Raw -Encoding UTF8 $Path
    return ($content -replace "`r`n", "`n").TrimEnd()
}

# Compute robust build hash
$FilesToHash = @(
    Get-NormalizedContent -Path $Dockerfile
    Get-NormalizedContent -Path "apt-packages.txt"
    Get-NormalizedContent -Path "r-packages.txt"
)

$HashAlgorithm = [System.Security.Cryptography.SHA256]::Create()
$JoinedContent = ($FilesToHash -join "`n")
$Bytes = [System.Text.Encoding]::UTF8.GetBytes($JoinedContent)
$HashBytes = $HashAlgorithm.ComputeHash($Bytes)
$BuildHash = [BitConverter]::ToString($HashBytes) -replace "-", ""

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

# Extract output_dir and synthpop_dir from YAML
function Get-YamlPathValue {
    param (
        [string]$YamlPath,
        [string]$Key
    )

    Write-Host "`n[DEBUG] YAML path: $YamlPath"
    Write-Host "[DEBUG] Key: $Key"

    $line = Select-String "^$Key\s*:" $YamlPath | Select-Object -First 1
    if ($line) {
        Write-Host "[DEBUG] Matched line: $($line.Line)"

        $value = ($line.Line -split ":\s*", 2)[1].Split("#")[0].Trim()
        Write-Host "[DEBUG] Extracted raw value: $value"

        try {
            $resolved = Resolve-Path $value -ErrorAction Stop
            Write-Host "[DEBUG] Resolved path: $($resolved.Path)"
        } catch {
            Write-Host "[ERROR] Could not resolve path: $value"
            return $null
        }

        # Detect Docker backend
        $dockerBackend = docker info --format '{{.OperatingSystem}}' 2>$null
        Write-Host "[DEBUG] Docker backend info: $dockerBackend"

        if ($dockerBackend -and $dockerBackend -match 'WSL') {
            # WSL2 backend
            $driveLetter = $resolved.Path.Substring(0,1).ToLower()
            $dockerPath = $resolved.Path -replace "^${driveLetter}:", "/mnt/$driveLetter"
            Write-Host "[DEBUG] Converted to WSL path: $dockerPath"
        } else {
            # Hyper-V backend
            $driveLetter = $resolved.Path.Substring(0,1).ToLower()
            $dockerPath = $resolved.Path -replace "^${driveLetter}:", "/run/desktop/mnt/host/$driveLetter"
            Write-Host "[DEBUG] Converted to Hyper-V path: $dockerPath"
        }

        $dockerPath = $dockerPath -replace '\\', '/'
        Write-Host "[DEBUG] Final Docker path: $dockerPath"

        return $dockerPath
    }

    Write-Host "[WARN] No matching line found for key: $Key"
    return $null
}

$outputDir = Get-YamlPathValue -YamlPath $SimDesignYaml -Key "output_dir"
$synthpopDir = Get-YamlPathValue -YamlPath $SimDesignYaml -Key "synthpop_dir"

if (-not $outputDir -or -not (Test-Path $outputDir)) {
    Write-Host "Error: output_dir path not found or invalid: $outputDir"
    Exit 1
}

if (-not $synthpopDir -or -not (Test-Path $synthpopDir)) {
    Write-Host "Error: synthpop_dir path not found or invalid: $synthpopDir"
    Exit 1
}

Write-Host "Mounting output_dir:    $outputDir"
Write-Host "Mounting synthpop_dir: $synthpopDir"

# Resolve project root directory (one level up)
$ProjectRoot = Resolve-Path "$ScriptDir\.."
$ProjectRoot = $ProjectRoot.Path -replace '\\', '/'

# Format paths for Docker
$outputDir = $outputDir -replace '\\', '/'
$synthpopDir = $synthpopDir -replace '\\', '/'

# Run Docker container
docker run -it `
    --mount type=bind,source="$ProjectRoot",target=/IMPACTncd_Japan `
    --mount type=bind,source="$outputDir",target=/IMPACTncd_Japan/output `
    --mount type=bind,source="$synthpopDir",target=/IMPACTncd_Japan/synthpop `
    --workdir /IMPACTncd_Japan `
    $ImageName `
    bash

Pop-Location
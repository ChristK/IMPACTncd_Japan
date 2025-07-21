---
title: "IMPACTncd_Japan Docker Setup - Complete Guide"
format:
  html:
    self-contained: true
---

# IMPACTncd_Japan Docker Setup - Complete Guide

This repository contains the Docker configuration, intelligent package management system, and cross-platform setup scripts for the **IMPACTncd Japan** project. The system provides robust, reproducible containerized environments with automatic package version management across Windows, macOS, and Linux.

## 🚀 Quick Start

Choose your platform and run the setup:

### Windows (PowerShell)
```powershell
.\create_env.ps1 [-SimDesignYaml <path\to\sim_design.yaml>] [-UseVolumes]
```

### macOS/Linux (Bash)
```bash
./create_env.sh [optional_path_to_sim_design.yaml] [--use-volumes]
```

---

## 🌟 New: Intelligent Package Management

This setup now includes an **intelligent package installation system** that automatically handles package version mismatches and maintains reproducibility.

### Key Features
- ✅ **Automatic Version Fallback**: When exact package versions aren't available, automatically installs the latest available version
- ✅ **Clear Reporting**: Shows exactly which packages were updated and why
- ✅ **Cross-Platform**: Works identically on Windows, macOS, and Linux
- ✅ **Easy Updates**: Provides tools to keep package versions current
- ✅ **Build Resilience**: Builds continue even when specific versions are unavailable

### How It Works

The intelligent installer (`install_packages.sh`) performs these steps:

1. **Try Exact Version**: Attempts to install each package with its pinned version from `apt-packages.txt`
2. **Automatic Fallback**: If a version fails:
   - Queries for the available candidate version
   - Installs the available version instead  
   - Records the change for reporting
3. **Clear Feedback**: Uses visual symbols (✓, ⚠, →) to show installation status
4. **Version Reporting**: Lists all packages that were updated with different versions

### Example Build Output
```
Starting intelligent package installation...
Processing package: git (requested version: 1:2.43.0-1ubuntu7.2)
⚠ Version 1:2.43.0-1ubuntu7.2 for git not available  
→ Installing available version: git=1:2.43.0-1ubuntu7.3
✓ Successfully installed git=1:2.43.0-1ubuntu7.3

==================================================
PACKAGE VERSION UPDATES DETECTED:
==================================================
The following packages were installed with different versions:
git=1:2.43.0-1ubuntu7.3

RECOMMENDED ACTION: Update your apt-packages.txt file with these versions
==================================================
```

---

## 🌐 Cross-Platform Compatibility

| Component | Windows | macOS | Linux | Notes |
|-----------|---------|--------|-------|-------|
| **Docker Build** | ✅ | ✅ | ✅ | Works identically |
| **Intelligent Installer** | ✅ | ✅ | ✅ | Runs in Docker container |
| **Package Management** | ✅ | ✅ | ✅ | Same format everywhere |
| **Update Scripts** | ✅ | ✅ | ✅ | Platform-specific utilities |

### Platform-Specific Tools

**Windows:**
- `update-apt-packages.ps1` - PowerShell script for package updates
- Full PowerShell integration

**macOS/Linux:**
- `update-apt-packages.sh` - Bash script for package updates  
- Native Unix tools integration

---

## 💾 Prerequisites

### Installing Docker

Before using the setup scripts, install Docker for your operating system:

- **Windows:** [Docker Desktop for Windows](https://docs.docker.com/desktop/install/windows-install/)
- **macOS:** [Docker Desktop for Mac](https://docs.docker.com/desktop/install/mac-install/)
- **Linux:** Follow instructions for your distribution:
  - [Ubuntu](https://docs.docker.com/engine/install/ubuntu/)
  - [Debian](https://docs.docker.com/engine/install/debian/)
  - [Fedora](https://docs.docker.com/engine/install/fedora/)
  - [CentOS](https://docs.docker.com/engine/install/centos/)

### Platform-Specific Setup

**Windows:**
- PowerShell 5.1+ (usually pre-installed)
- Docker Desktop running

**macOS:**
```bash
# Install Docker Desktop and ensure bash is available
brew install docker coreutils  # coreutils provides gsha256sum
```

**Linux:**
```bash
# Ubuntu/Debian
sudo apt-get update && sudo apt-get install docker.io

# Add user to docker group (optional)
sudo usermod -aG docker $USER
```

---

## 🐳 Docker Configuration

### Container Specifications
- **Image Name:** `impactncd-japan-r-prerequisite:latest`
- **Base Image:** [rocker/r-ver:4.5.1](https://hub.docker.com/r/rocker/r-ver)
- **R Version:** 4.5.1 with packages frozen as of July 20, 2025
- **Package Manager:** [RStudio Package Manager](https://packagemanager.posit.co/client/#/)

### File Structure
- **`Dockerfile.prerequisite`**: Main Docker configuration with intelligent installer
- **`apt-packages.txt`**: System packages with pinned versions
- **`r-packages.txt`**: R packages list
- **`install_packages.sh`**: Intelligent package installer script
- **`entrypoint.sh`**: Container entrypoint for dynamic user creation

---

## � Usage Guide

### Basic Docker Build
```bash
# All platforms
docker build -f Dockerfile.prerequisite -t impactncd-japan .
```

### Verbose Build (See Package Updates)
```bash
# Windows PowerShell
docker build -f Dockerfile.prerequisite --progress=plain --no-cache -t my-image . 2>&1 | Select-String -Pattern "(Processing package|Successfully installed|Version.*not available|Installing available|PACKAGE VERSION)"

# macOS/Linux Bash  
docker build -f Dockerfile.prerequisite --progress=plain --no-cache -t my-image . 2>&1 | grep -E "(Processing package|Successfully installed|Version.*not available|Installing available|PACKAGE VERSION)"
```

### Update Package Versions

When you see package version updates in build output:

**Windows:**
```powershell
# Interactive update
.\update-apt-packages.ps1 -Interactive

# From build log file
.\update-apt-packages.ps1 -BuildLogFile "build.log"
```

**macOS/Linux:**
```bash
# Make executable (first time only)
chmod +x update-apt-packages.sh

# Interactive update
./update-apt-packages.sh --interactive

# From build log file  
./update-apt-packages.sh --file build.log

# Pipe directly from build
docker build ... 2>&1 | ./update-apt-packages.sh
```

---

## 🏗 Environment Setup Modes

### Bind Mount Mode (Default)
- Direct bind mounts between host and container
- Real-time interaction between host and container
- More flexible but potentially slower

### Volume Mode (Recommended for Large Simulations)
Use `--use-volumes` (Bash) or `-UseVolumes` (PowerShell):
- Project directory copied to Docker-managed volume
- Better performance for large datasets
- Includes post-simulation sync back to host

---

## 📁 Directory Mounting

| Host Path | Container Mount | Description |
|-----------|-----------------|-------------|
| **Project Root** (above docker_setup) | `/IMPACTncd_Japan` | Main project directory |
| `output_dir` from `sim_design.yaml` | `/output` | Simulation outputs |
| `synthpop_dir` from `sim_design.yaml` | `/synthpop` | Synthetic population data |

---

## 🔄 Development Workflow

The same workflow applies across all platforms:

1. **Edit Code**: Modify R code or dependencies
2. **Build Image**: Run `docker build` command  
3. **Check Updates**: Look for package version update messages
4. **Update Versions**: Use appropriate update script for your platform
5. **Rebuild**: Build again with updated versions for reproducibility

### Multi-Platform Team Example
```bash
# Developer on macOS
./update-apt-packages.sh --interactive
git add apt-packages.txt
git commit -m "Update git package version"
git push

# Developer on Windows
git pull
.\update-apt-packages.ps1 -Interactive  # Shows no updates needed

# Developer on Linux  
git pull
./update-apt-packages.sh --interactive  # Shows no updates needed
```

---

## 🛠 Build and Push Scripts

Build and optionally push Docker images:

**Prerequisite Container:**
- Linux/macOS: `./build_and_push_prerequisite.sh [--push]`
- Windows: `build_and_push_prerequisite.ps1 [-Push]`

**IMPACTncd Container:**
- Linux/macOS: `./build_and_push_IMPACTncd.sh [--push]`
- Windows: `build_and_push_IMPACTncd.ps1 [-Push]`

---

## 🧼 Cleanup

**Remove Docker Image:**
```bash
docker rmi impactncd-japan-r-prerequisite:latest
```

**Prune Unused Resources:**
```bash
docker system prune
```

---

## 🔍 Troubleshooting

### General Docker Issues
- **Check Docker Status:** Run `docker info`
- **Expected:** Detailed Docker installation info without errors
- **Common Error:** "Cannot connect to the Docker daemon"

**Solutions:**
- **Windows/macOS:** Ensure Docker Desktop is running
- **Linux:** Check service with `sudo systemctl status docker`
  - Start if needed: `sudo systemctl start docker`
  - Add user to docker group: `sudo usermod -aG docker $USER` (then logout/login)

### Platform-Specific Issues

**Windows:**
- Use PowerShell (not Command Prompt) for best compatibility
- May need to adjust execution policy: `Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass`
- Check WSL2 configuration for Docker Desktop

**macOS:**
- Install required tools: `brew install coreutils`
- Make scripts executable: `chmod +x update-apt-packages.sh`
- Check Docker Desktop permissions in System Preferences

**Linux:**
- Ensure Docker service is running: `sudo systemctl start docker`
- Make scripts executable: `chmod +x update-apt-packages.sh`
- May need `sudo docker` if user not in docker group

### Package Update Issues
- If specific versions fail, the intelligent installer automatically uses available versions
- Check build output for "PACKAGE VERSION UPDATES DETECTED" section
- Update `apt-packages.txt` with new versions for reproducibility

---

## 📦 Package Management Details

### System Packages (`apt-packages.txt`)
Contains Ubuntu packages with pinned versions:
```
automake=1:1.16.5-1.3ubuntu1
cmake=3.28.3-1build7
git=1:2.43.0-1ubuntu7.3
# ... more packages
```

### R Packages (`r-packages.txt`)  
R packages from CRAN snapshot (July 20, 2025):
```
data.table
ggplot2
dplyr
# ... more packages
```

### Version Update Workflow
1. **Automatic Detection**: Build detects unavailable versions
2. **Clear Reporting**: Shows which packages were updated
3. **Manual Update**: Update package files with new versions
4. **Automation**: Use provided scripts to streamline updates

---

## 🎯 Benefits

1. **Reproducible Builds**: Version pinning where possible, intelligent fallback when needed
2. **Cross-Platform**: Identical behavior on Windows, macOS, and Linux
3. **Resilient**: Builds don't fail due to outdated package versions
4. **Clear Feedback**: Visual indicators show exactly what happened
5. **Easy Maintenance**: Automated tools for version management
6. **Team Collaboration**: Consistent environment across different platforms

---

## 📝 Reproducibility Notes

While pinning versions with `<package>=<version>` is the standard apt method, package repositories don't guarantee indefinite availability of historical versions. Security updates and repository cleanup can affect older versions.

**Advanced Options for Maximum Reproducibility:**

1. **Repository Mirroring**: Create local mirrors of Ubuntu repositories at specific points in time (complex but complete control)

2. **Multi-Stage Builds with .deb Files**: Store specific .deb files alongside Dockerfile and install using `dpkg -i` (harder dependency management)

The intelligent package system balances reproducibility with practical build reliability by maintaining version pinning where possible while gracefully handling unavailable versions.

---

## 📬 Support

For assistance or issues:
- Open an issue in this repository
- Contact project maintainers
- Check troubleshooting section above

---

## 🏷 Project Details

- **Supported Branch:** master of IMPACTncd Japan model
- **R Version:** 4.5.1
- **Package Snapshot:** July 20, 2025
- **Base Image:** rocker/r-ver:4.5.1
- **Supported Platforms:** Windows 10/11, macOS, Linux (Ubuntu/Debian/CentOS/Fedora)

<!-- Generate HTML documentation: pandoc README.md -s -o README.html --embed-resources --standalone --toc --toc-depth=3 --number-sections --css=github.css --highlight-style=github --metadata title="IMPACTncd_Japan Docker Setup Guide" --template=template.html --include-in-header=header.html --include-before-body=before.html --include-after-body=after.html --filter pandoc-crossref --citeproc --bibliography=references.bib --csl=style.csl --metadata link-citations=true --metadata linkReferences=true --metadata nameInLink=true --metadata figureTitle="Figure" --metadata tableTitle="Table" --metadata listingTitle="Listing" --from markdown+smart+raw_html+fenced_divs+bracketed_spans --to html5 --standalone --self-contained --embed-resources --resource-path=.:assets:images --extract-media=./media --strip-comments --email-obfuscation=references --tab-stop=4 --preserve-tabs --indented-code-classes=bash,powershell,yaml,dockerfile --columns=80 --wrap=auto --ascii --reference-links --reference-location=block --atx-headers --listings --pdf-engine=pdflatex --variable mainfont="DejaVu Serif" --variable monofont="DejaVu Sans Mono" --variable geometry:margin=1in --metadata lang=en-US --metadata dir=ltr --metadata documentclass=article --metadata classoption=12pt --metadata papersize=letter --metadata fontsize=12pt --metadata linestretch=1.2 --metadata indent=true --metadata secnumdepth=3 --metadata toc=true --metadata toc-depth=3 --metadata lot=false --metadata lof=false --metadata thanks="IMPACTncd_Japan Project Documentation" --metadata author="IMPACTncd_Japan Team" --metadata date=today --metadata subject="Docker Setup Guide" --metadata keywords="Docker, R, IMPACTncd, Cross-Platform, Package Management" --metadata description="Complete guide for setting up IMPACTncd_Japan Docker environment with intelligent package management across Windows, macOS, and Linux platforms" --metadata creator="Pandoc" --metadata producer="IMPACTncd_Japan Documentation System" --fail-if-warnings --verbose -->

*This documentation combines intelligent package management, cross-platform compatibility, and comprehensive setup instructions for the IMPACTncd_Japan Docker environment.*
     - **Project Volume:**  
       The entire project directory (one level above `docker_setup`) is copied into a Docker-managed volume.
     - **Output and Synthpop Volumes:**  
       Created and pre-populated from the local folders.
     - **Running the Container:**  
       The container is launched with these volumes mounted.
     - **Post-Simulation Sync:**  
       After exiting, the script uses an `rsync-alpine` image to sync the output and synthpop volumes back to their respective local directories.
     - **Cleanup:**  
       Volumes are removed after syncing.
     


### 🚀 Quick Start

#### Linux/macOS (Bash)

```bash
./create_env.sh [optional_path_to_sim_design.yaml] [--use-volumes]
```

#### Windows (PowerShell)

```powershell
.\create_env.ps1 [-SimDesignYaml <path	o\sim_design.yaml>] [-UseVolumes]
```


After installation, ensure the Docker service/daemon is running before executing the setup scripts.

## 🔍 Directory Mounting Summary

| Host Path                                         | Mounted to inside Container      |
|---------------------------------------------------|----------------------------------|
| **Project Root** (one level above `docker_setup`) | `/IMPACTncd_Japan`               |
| `output_dir` from `sim_design.yaml`               | `/output`                        |
| `synthpop_dir` from `sim_design.yaml`             | `/synthpop`                      |

## 🐳 Docker Image Details

- **Image Name:** `impactncd-japan-r-prerequisite:latest`
- **Base Image:** [`rocker/r-ver`](https://hub.docker.com/r/rocker/r-ver)
- **System Packages:** Listed in [`apt-packages.txt`](./apt-packages.txt)
- **R Packages:** Listed in [`r-packages.txt`](./r-packages.txt)

## 🧼 Cleanup

- **Remove the Docker Image:**

```bash
docker rmi impactncd-japan-r-prerequisite:latest
```

- **Prune Unused Containers/Images:**

```bash
docker system prune
```

## ❓ Troubleshooting

- **Docker Issues:** Ensure the Docker service (daemon) is running and accessible.
  - **How to check:** Run `docker info` in your terminal. 
  - **Expected Output:** You should see detailed information about your Docker installation (Server Version, Storage Driver, etc.) without any error messages.
  - **Common Errors:** If you see "Cannot connect to the Docker daemon" or similar errors, it means Docker isn't running or your user doesn't have permission to access it.
  - **What to do:** 
    - **Windows/macOS:** Make sure Docker Desktop is running (check the system tray or application list).
    - **Linux:** Check the service status with `sudo systemctl status docker`. If it's inactive, start it with `sudo systemctl start docker`. You might also need to add your user to the `docker` group (`sudo usermod -aG docker $USER`) and then log out and back in for the change to take effect.
- **macOS Users:** This script requires `gsha256sum` for calculating file hashes. This utility is part of the `coreutils` package. 
    - **If you have [Homebrew](https://brew.sh/) installed:** Run `brew install coreutils` to install it.
    - **If you don't have Homebrew:** You can install it by following the instructions on the [official Homebrew website](https://brew.sh/). Typically, you run a command like this in your macOS Terminal:
      ```bash
      /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
      ```
      After installing Homebrew, run `brew install coreutils`.
- **Windows Users:** Prefer using the PowerShell script (`create_env.ps1`). You might need to adjust PowerShell's execution policy to allow running local scripts like this one. If you encounter an error running the script, try executing `Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass` in your PowerShell terminal first. This command temporarily allows running unsigned scripts for the current session.

## 📬 Need Help?

For assistance or issues, contact the project maintainers or open an issue in this repository.

## 🛠 Build and Push to Docker Hub

Use the provided build scripts to build and optionally push the Docker image:

- **Prerequisite Container:**
  - Linux/macOS: `./build_and_push_prerequisite.sh [--push]`
  - Windows: `build_and_push_prerequisite.ps1 [-Push]`

- **IMPACTncd Container:**
  - Linux/macOS: `./build_and_push_IMPACTncd.sh [--push]`
  - Windows: `build_and_push_IMPACTncd.ps1 [-Push]`

## A note regarding reproducibility

While pinning versions with <package>=<version> is the standard apt method, package repositories (even the official Ubuntu ones) don't guarantee that every historical version will be available indefinitely. Security updates might replace older versions, or repositories might be cleaned up over time.

Below are the two options for ensuring reproducibility of the system libraries but at the moment their complexity hardly justify the effort.

1. Mirroring Repositories: The most robust but complex solution is to create your own local mirror of the Ubuntu repositories at a specific point in time. You would then configure your Dockerfile to use your mirror instead of the public ones. This gives you complete control but requires significant storage and maintenance. Tools like apt-mirror or debmirror can be used for this.

2. Multi-Stage Builds with .deb Files: In a separate Dockerfile stage or a temporary container, install the desired versions.
Copy the downloaded .deb files (usually found in archives) out of that temporary stage/container.
Store these .deb files alongside your Dockerfile. In your main Dockerfile, COPY these .deb files in and use dpkg -i /path/to/*.deb || apt-get install -f -y to install them. This forces the specific versions but makes dependency management much harder, as you also need to capture all dependencies. apt-get install -f -y attempts to fix broken dependencies after dpkg -i.

<!-- pandoc README.md -s -o README.html --embed-resources --standalone -->
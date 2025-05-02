# IMPACTncd_Japan Prerequisite Dockerfile and Setup

This repository contains the Dockerfile and the scripts used to build and run the prerequisite container for the IMPACTncd Japan project. The container is based on Ubuntu and includes R version 4.4.3 with package versions frozen as of 31/03/2025, using the [RStudio Package Manager](https://packagemanager.posit.co/client/#/). Update R packages by editing `r-packages.txt` and system libraries by editing `apt-packages.txt`, then rebuild the image as needed. You can find the current version of a system library in an ubuntu system using i.e.`apt-cache policy libxml2-dev` or check the version available in the base R image using `docker run --rm rocker/r-ver:4.4.3 bash -c "apt-get update && apt-cache policy libxml2-dev"`.

This Docker container supports the branch **master** of the IMPACTncd Japan model.

## 💾 Installing Docker

Before you can use the setup scripts, you need to have Docker installed on your system. Follow the official instructions for your operating system:

- **Windows:** Install [Docker Desktop for Windows](https://docs.docker.com/desktop/install/windows-install/).
- **macOS:** Install [Docker Desktop for Mac](https://docs.docker.com/desktop/install/mac-install/).
- **Linux:** Follow the instructions for your specific distribution:
    - [Ubuntu](https://docs.docker.com/engine/install/ubuntu/)
    - [Debian](https://docs.docker.com/engine/install/debian/)
    - [Fedora](https://docs.docker.com/engine/install/fedora/)
    - [CentOS](https://docs.docker.com/engine/install/centos/)
    - For other distributions, refer to the [Docker Engine installation overview](https://docs.docker.com/engine/install/).

## 🐳 Docker Setup for IMPACTncd Japan

This directory contains the Docker configuration and helper scripts required to build a containerized environment for the **IMPACTncd Japan** project. Two versions of the setup script are provided:

- **Bash Script**: [`create_env.sh`](./create_env.sh)  
- **PowerShell Script**: [`create_env.ps1`](./create_env.ps1)

Both scripts share the same functionality, with the following workflow:

### Workflow Summary

1. **Configuration and Build Input Check:**
   - Accept an optional path to a `sim_design.yaml` file.
   - Extract key paths (`output_dir` and `synthpop_dir`) from the YAML.
   - Compute a hash from build inputs (Dockerfile, apt-packages.txt, r-packages.txt) to determine if the Docker image must be rebuilt.

2. **Operation Modes:**  
   The scripts support two modes:

   - **Bind Mount Mode:**  
     Default mode using direct bind mounts. This is a flexible mode as it allows realtime interaction between the host and the container. The downside is that it can be slower and less portable, especially on Windows and macOS.
   
   - **Volume Mode (Recommended for large simulations):**  
     When using the `--use-volumes` (Bash) or `-UseVolumes` (PowerShell) flag:
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

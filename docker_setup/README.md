
# IMPACTncd_Japan Prerequisite Dockerfile and Setup

This repository contains the Dockerfile and the scripts used to build and run the prerequisite container for the IMPACTncd Japan project. The container is based on Ubuntu and includes R version 4.4.3 with package versions frozen as of 31/03/2025, using the [RStudio Package Manager](https://packagemanager.posit.co/client/#/). Update R packages by editing `r-packages.txt` and system libraries by editing `apt-packages.txt`, then rebuild the image as needed.

This Docker container supports the branch **master** of the IMPACTncd Japan model.

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
   
   - **Volume Mode (Recommended for macOS and Windows):**  
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
     
   - **Bind Mount Mode:**  
     Default mode using direct bind mounts.

### 🚀 Quick Start

#### Linux/macOS (Bash)

```bash
./create_env.sh [optional_path_to_sim_design.yaml] [--use-volumes]
```

#### Windows (PowerShell)

```powershell
.\create_env.ps1 [-SimDesignYaml <path	o\sim_design.yaml>] [-UseVolumes]
```

## 🔍 Directory Mounting Summary

| Host Path                               | Mounted to inside Container                |
|-----------------------------------------|--------------------------------------------|
| **Project Root** (one level above `docker_setup`) | `/IMPACTncd_Japan` |
| `output_dir` from `sim_design.yaml`     | `/IMPACTncd_Japan/output`                   |
| `synthpop_dir` from `sim_design.yaml`   | `/IMPACTncd_Japan/synthpop`                 |

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

## 🛠 Build and Push

Use the provided build scripts to build and optionally push the Docker image:

- **Prerequisite Container:**
  - Linux/macOS: `./build_and_push_prerequisite.sh [--push]`
  - Windows: `build_and_push_prerequisite.ps1 [-Push]`

- **IMPACTncd Container:**
  - Linux/macOS: `./build_and_push_IMPACTncd.sh [--push]`
  - Windows: `build_and_push_IMPACTncd.ps1 [-Push]`

## ❓ Troubleshooting

- **Docker Issues:** Ensure Docker Desktop (or Docker Engine) is running.
- **macOS Users:** Install `coreutils` if encountering hash computation issues.
- **Windows Users:** Prefer PowerShell and adjust execution policy if needed.

## 📬 Need Help?

For assistance or issues, contact the project maintainers or open an issue in this repository.

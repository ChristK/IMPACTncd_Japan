# IMPACTncd_Japan prerequisite dockerfile

This repository contains the Dockerfile used to build the prerequisite container for the IMPACTncd model. The container is based on Ubuntu and includes R version 4.4.3 with package versions frozen as of 31/03/2025, using the [RStudio Package Manager](https://packagemanager.posit.co/client/#/). You can update the required R packages by editing the file `r-packages.txt` and running the build script. Similarly, you can update the required operating system libraries by editing the file `apt-packages.txt` and running the build script. You can check the system prerequisites for Ubuntu 24.04 (Noble)  for any additional package you may want to add `r-packages.txt` [here](https://packagemanager.posit.co/client/#/repos/cran/packages/overview?search=) and then if necessary add the requires system library to `apt-packages.txt`.To find the version of the system library, for full reproducibility, you can use the command `apt-cache policy <package_name>` from a terminal within the container.

This Docker container is required for the branch 'master' of the IMPACTncd_Japan model.

## 🐳 Docker Setup for IMPACTncd Japan

This directory contains the Docker configuration and scripts needed to build and run a containerized environment for the **IMPACTncd Japan** project.

The main script, [`create_env.sh`](./create_env.sh), builds a Docker image with the necessary system and R packages, and runs a container with the project directory mounted for immediate use.

---

### 🚀 Quick Start (Linux/macOS)

```bash
cd docker_setup
./create_env.sh
```

> On macOS, install `coreutils` first (if needed):
> ```bash
> brew install coreutils
> ```

---

### 🪟 Quick Start (Windows - PowerShell)

Use the PowerShell script [`create_env.ps1`](./create_env.ps1):

```powershell
cd docker_setup
.\create_env.ps1
```

> If you encounter an execution policy error, run:
> ```powershell
> Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass
> ```

---

## 📦 What the create_env.* Script Does

1. Computes a hash from:
   - `Dockerfile.prerequisite`
   - `apt-packages.txt`
   - `r-packages.txt`

2. Compares the hash to a saved version in `.docker_build_hash`.
3. Rebuilds the Docker image **only if** relevant files changed.
4. Mounts the parent directory (`../`) into the container at `/IMPACTncd_Japan`.
5. Launches the container with an interactive shell.

---

## 🔍 Directory Mounting

The parent directory of `docker_setup` is mounted into the container at:

```
/IMPACTncd_Japan
```

This allows you to access and run R scripts inside the container like so:

```bash
Rscript /IMPACTncd_Japan/scripts/my_analysis.R
```

---

## 🐳 Docker Image

- **Image name:** `impactncd-japan-r-prerequisite:latest`
- **Base image:** [`rocker/r-ver`](https://hub.docker.com/r/rocker/r-ver)
- **System packages:** listed in [`apt-packages.txt`](./apt-packages.txt)
- **R packages:** listed in [`r-packages.txt`](./r-packages.txt)

---

## 🧼 Clean-Up

To remove the Docker image:

```bash
docker rmi impactncd-japan-r-prerequisite:latest
```

To prune unused containers/images:

```bash
docker system prune
```

---


## Build and Push Using Provided Scripts

You can use the provided scripts:

You can use the provided scripts:

- **Prerequisite Container:**
  - On Linux or macOS: `./build_and_push_prerequisite.sh`
    - Use the `--push` argument to push the Docker image to Docker Hub after building.
  - On Windows: `build_and_push_prerequisite.ps1`
    - Use the `-Push` argument to push the Docker image to Docker Hub after building.

- **IMPACTncd Container:**
  - On Linux or macOS: `./build_and_push_IMPACTncd.sh`
    - Use the `--push` argument to push the Docker image to Docker Hub after building.
  - On Windows: `build_and_push_IMPACTncd.ps1`
    - Use the `-Push` argument to push the Docker image to Docker Hub after building.

## ❓ Troubleshooting

- **Docker not found?** Make sure Docker Desktop is running.
- **On macOS:** You may need `coreutils` (for `gsha256sum`).
- **On Windows:** Use PowerShell, not Command Prompt.

---

## 📬 Need Help?

If you encounter issues, please contact the project maintainers or raise an issue in this repository.

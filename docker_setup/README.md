# IMPACTncd_Japan Docker Setup

Cross-platform Docker configuration and setup scripts for the **IMPACTncd Japan** project. Two entry points cover the common workflows:

- **`setup_user_docker_env.{sh,ps1}`** — pulls a pre-built image from Docker Hub and runs the model. Most users want this.
- **`setup_dev_docker_env.{sh,ps1}`** — builds the prerequisite image locally from `Dockerfile.prerequisite.IMPACTncdJPN` and mounts the project source into the container. Use this when you're modifying the model code or its dependency lists.

---

## 💾 Prerequisites

Install Docker for your platform:

- **Windows:** [Docker Desktop for Windows](https://docs.docker.com/desktop/install/windows-install/)
- **macOS:** [Docker Desktop for Mac](https://docs.docker.com/desktop/install/mac-install/)
- **Linux:** [Ubuntu](https://docs.docker.com/engine/install/ubuntu/) · [Debian](https://docs.docker.com/engine/install/debian/) · [Fedora](https://docs.docker.com/engine/install/fedora/) · [CentOS](https://docs.docker.com/engine/install/centos/)

Additional per-platform requirements:

**Windows**
- PowerShell 5.1+ (preinstalled).
- If your YAML config uses Windows-style absolute paths (e.g. `output_dir: C:/data/...`), the scripts auto-translate them to the correct WSL mount path via `wsl.exe wslpath -u`. This works correctly even when `/etc/wsl.conf` sets a non-default `[automount] root` (e.g. `/mnt/host/`).

**macOS**
```bash
brew install coreutils   # provides gsha256sum
```

**Linux**
```bash
sudo apt-get update && sudo apt-get install docker.io      # if not already installed
sudo usermod -aG docker $USER                              # then log out and back in
```

---

## 🚀 Quick Start — running the model

### Windows (PowerShell)

```powershell
# Default: pulls chriskypri/impactncdjpn:main
.\setup_user_docker_env.ps1

# Specific Docker Hub tag
.\setup_user_docker_env.ps1 -Tag "v1.2.3"

# Locally built image (see "Building images locally" below)
.\setup_user_docker_env.ps1 -Tag "local"

# Custom YAML
.\setup_user_docker_env.ps1 -SimDesignYaml "C:\path\to\custom_sim_design.yaml"

# Mount a scenarios directory at /IMPACTncd_Japan/scenarios in the container
.\setup_user_docker_env.ps1 -ScenariosDir "C:\path\to\scenarios"

# Use Docker volumes instead of bind mounts (recommended on Windows/macOS)
.\setup_user_docker_env.ps1 -UseVolumes

# Combine options
.\setup_user_docker_env.ps1 -Tag "v1.2.3" -ScenariosDir "..\scenarios" -UseVolumes
```

### macOS / Linux (Bash)

The bash script uses **flag-style** arguments (`-Tag value`), not positional ones — copy the form below verbatim.

```bash
# Default: pulls chriskypri/impactncdjpn:main
./setup_user_docker_env.sh

# Specific Docker Hub tag
./setup_user_docker_env.sh -Tag v1.2.3

# Locally built image
./setup_user_docker_env.sh -Tag local

# Custom YAML
./setup_user_docker_env.sh -SimDesignYaml /path/to/custom_sim_design.yaml

# Mount a scenarios directory at /IMPACTncd_Japan/scenarios in the container
./setup_user_docker_env.sh -ScenariosDir /path/to/scenarios

# Use Docker volumes
./setup_user_docker_env.sh --UseVolumes

# Combine options
./setup_user_docker_env.sh -Tag v1.2.3 -ScenariosDir /path/to/scenarios --UseVolumes
```

### Parameters

Both scripts accept the same options. Note that `--UseVolumes` is a double-dash flag in bash but `-UseVolumes` in PowerShell — that's a PowerShell convention, not a typo.

| PowerShell | Bash | Description |
|---|---|---|
| `-Tag <name>` | `-Tag <name>` | Image tag. Default: `main`. |
| `-ScenariosDir <path>` | `-ScenariosDir <path>` | Optional. Mounted at `/IMPACTncd_Japan/scenarios`. |
| `-SimDesignYaml <path>` | `-SimDesignYaml <path>` | Path to YAML config. Default: `../inputs/sim_design.yaml`. |
| `-UseVolumes` | `--UseVolumes` | Use Docker volumes instead of bind mounts. |

### Image selection logic

| `-Tag` value | Image used | Source |
|---|---|---|
| `main` (default) | `chriskypri/impactncdjpn:main` | Docker Hub |
| `local` | `impactncdjpn:local` | Your local Docker registry — build first (see below) |
| anything else, e.g. `v1.2.3` | `chriskypri/impactncdjpn:v1.2.3` | Docker Hub |

Available remote tags: https://hub.docker.com/r/chriskypri/impactncdjpn/tags

---

## 📁 Mount points

| Host | Container | Description |
|---|---|---|
| (built into image) | `/IMPACTncd_Japan` | Project source. The image already contains it; no host project mount is needed. |
| `output_dir` from YAML | `/outputs` | Simulation outputs. |
| `synthpop_dir` from YAML | `/synthpop` | Synthetic population data. |
| `-ScenariosDir` (optional) | `/IMPACTncd_Japan/scenarios` | Scenario definitions. |

### Bind mount mode (default)
Host directories are mounted directly. Real-time visibility, lower overhead. Recommended on Linux.

### Volume mode (`-UseVolumes` / `--UseVolumes`)
The script creates Docker-managed volumes for `output` and `synthpop`, runs the container against them, then rsyncs the contents back to the host directories on exit and removes the volumes. Recommended on Windows and macOS where bind mounts have higher I/O overhead.

---

## 🛠 Building images locally

### Developer workflow — `setup_dev_docker_env.{sh,ps1}`

This script builds `prerequisite.impactncdjpn:local` from `Dockerfile.prerequisite.IMPACTncdJPN` and runs the container with your project source mounted at `/IMPACTncd_Japan`. It auto-rebuilds the image when any of `Dockerfile.prerequisite.IMPACTncdJPN`, `apt-packages.txt`, `r-packages.txt`, or `entrypoint.sh` changes (tracked via a hash file in this directory).

```bash
# Linux/macOS
./setup_dev_docker_env.sh                              # bind-mount mode
./setup_dev_docker_env.sh --UseVolumes                 # volume mode
./setup_dev_docker_env.sh ../inputs/sim_design_test.yaml

# Windows
.\setup_dev_docker_env.ps1
.\setup_dev_docker_env.ps1 -UseVolumes
.\setup_dev_docker_env.ps1 -SimDesignYaml "..\inputs\sim_design_test.yaml"
```

The dev script does not accept `-Tag` — its image name is fixed.

### Producing `impactncdjpn:local` for `-Tag local`

The user script's `-Tag local` route looks specifically for an image named `impactncdjpn:local`. Build it with `docker_build_push.{sh,ps1}` (run from the `docker_setup/` directory):

```bash
# Linux/macOS
./docker_build_push.sh --image-name impactncdjpn --image-tag local Dockerfile.IMPACTncdJPN

# Windows
.\docker_build_push.ps1 -ImageName impactncdjpn -ImageTag local Dockerfile.IMPACTncdJPN
```

Add `--push` / `-Push` to also publish to Docker Hub. That requires `DOCKERHUB_USERNAME` and `DOCKERHUB_TOKEN` in the environment or in a `.env` file alongside the script.

### Verbose build output

Useful when you want to see which apt versions the intelligent installer is substituting:

```bash
# Bash
docker build -f Dockerfile.prerequisite.IMPACTncdJPN --progress=plain --no-cache -t my-image . 2>&1 \
  | grep -E "(Processing package|Successfully installed|Version.*not available|Installing available|PACKAGE VERSION)"
```

```powershell
# PowerShell
docker build -f Dockerfile.prerequisite.IMPACTncdJPN --progress=plain --no-cache -t my-image . 2>&1 `
  | Select-String -Pattern "(Processing package|Successfully installed|Version.*not available|Installing available|PACKAGE VERSION)"
```

### Updating pinned package versions

When a build reports unavailable apt versions, refresh `apt-packages.txt`:

```bash
# Linux/macOS
./update-apt-packages.sh --interactive
./update-apt-packages.sh --file build.log
docker build ... 2>&1 | ./update-apt-packages.sh    # pipe directly from a build
```

```powershell
# Windows
.\update-apt-packages.ps1 -Interactive
.\update-apt-packages.ps1 -BuildLogFile "build.log"
```

---

## ❓ Troubleshooting

**"Cannot connect to the Docker daemon"**
- Windows / macOS: ensure Docker Desktop is running.
- Linux: `sudo systemctl start docker`.
- Verify with `docker info`.

**"Failed to pull Docker image"**
- Confirm the tag exists at https://hub.docker.com/r/chriskypri/impactncdjpn/tags.
- For private images: `docker login`.
- To verify a *local* image exists, use `docker image inspect impactncdjpn:local` — `docker pull` only works for remote images.

**Windows — "Execution policy" error**
```powershell
Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass
```

**Windows + WSL — bind mount paths**
The scripts auto-translate Windows-style absolute paths (`C:/...`) via `wsl.exe wslpath -u`. If you see a warning that wsl.exe wasn't found and the script is falling back to the legacy `/c/...` form, your daemon is likely Hyper-V-backed or running on a host without WSL — either install WSL or use POSIX paths in the YAML.

**Linux — "permission denied" on the Docker socket**
```bash
sudo usermod -aG docker $USER       # then log out and back in
```

**macOS — `gsha256sum: command not found`**
```bash
brew install coreutils
```

**Specific apt versions failing**
The intelligent installer in `Dockerfile.prerequisite.IMPACTncdJPN` automatically substitutes the closest available version and prints a `PACKAGE VERSION UPDATES DETECTED` block at the end of the build. Run `update-apt-packages.{sh,ps1} --interactive` to fold those substitutions back into `apt-packages.txt`.

---

## 🧼 Cleanup

```bash
# Images produced by the user-facing workflow
docker rmi chriskypri/impactncdjpn:main impactncdjpn:local

# Image produced by the dev-facing workflow
docker rmi prerequisite.impactncdjpn:local

# Image produced by docker_build_push.sh with default --image-name
docker rmi impactncd-japan-r-prerequisite:local

# Prune stopped containers and dangling images
docker system prune
```

---

## 📦 Package management details

### System packages — `apt-packages.txt`

Pinned Ubuntu package versions, e.g.:
```
automake=1:1.16.5-1.3ubuntu1
cmake=3.28.3-1build7
git=1:2.43.0-1ubuntu7.3
```

The intelligent installer in `Dockerfile.prerequisite.IMPACTncdJPN` falls back to the nearest available version when the pinned one has been rotated out of the apt repository, and prints what it substituted.

### R packages — `r-packages.txt`

Installed from the Posit Package Manager snapshot frozen at **2025-07-20**. The snapshot URL pins the versions; the file itself just lists package names.

### Reproducibility caveats

Apt-pinned versions are not guaranteed forever — Ubuntu rotates older versions out during security maintenance. The intelligent installer trades strict reproducibility for build reliability. For stricter reproducibility, either (a) mirror Ubuntu repositories at a fixed point in time, or (b) vendor the `.deb` files into the repo and install them with `dpkg -i`.

---

## 🏷 Project details

- Supported branch: `master`
- Base image: [`rocker/r-ver:4.5.1`](https://hub.docker.com/r/rocker/r-ver)
- R version: 4.5.1
- R package snapshot: 2025-07-20
- Supported platforms: Windows 10/11 (including PowerShell-in-WSL), macOS, Linux (Ubuntu / Debian / Fedora / CentOS)

---

## 📬 Need help?

Open an issue in the repository or contact the project maintainers.

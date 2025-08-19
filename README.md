# IMPACT NCD Japan microsimulation

--------------------------------------------------------------------------------

IMPACT NCD Japan is an implementation of the IMPACTncd framework, developed by Chris
Kypridemos with contributions from Peter Crowther (Melandra Ltd), Maria
Guzman-Castillo, Amandine Robert, and Piotr Bandosz. 

Copyright (C) 2018-2022 University of Liverpool, Chris Kypridemos

IMPACTncd_Japan is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 3 of the License, or (at your option) any later
version. This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details. You should have received a copy of the GNU General Public License along
with this program; if not, see <http://www.gnu.org/licenses/> or write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

## IMPACTncd_Japan deployment instructions


## What this model is

IMPACT NCD Japan is a population microsimulation model built on the IMPACTncd framework. It simulates Japanese adults’ lifecourse, exposure trajectories (e.g., smoking, BMI, blood pressure), disease onset (CHD, stroke, diabetes, etc.), and mortality. It’s designed to evaluate baseline trends and policy scenarios, producing lifecourse outputs, summary tables, and plots.

Typical uses:
- Run a baseline to quantify trends in incidence, prevalence, and mortality.
- Simulate policy scenarios by changing exposure trends or risk relations and compare outcomes.

Outputs are written under your configured outputs directory (by default folders such as outputs/, summaries/, tables/, plots/).

## Run with Docker (Windows, macOS, Linux)

You don’t need to install R or system dependencies locally. We provide ready-to-run images and helper scripts.

### 1) Install Docker

- Windows (Docker Desktop): https://docs.docker.com/desktop/install/windows-install/
- macOS (Docker Desktop): https://docs.docker.com/desktop/install/mac-install/
- Linux (Docker Engine): https://docs.docker.com/engine/install/
	- Linux post-install (manage Docker as a non-root user): https://docs.docker.com/engine/install/linux-postinstall/

After installing, ensure Docker is running (Docker Desktop app on Windows/macOS; system service on Linux).

### 2) Get the code

Clone or download this repository to your machine.

### 3) Use the helper script for your OS

The images already contain the project directory /IMPACTncd_Japan inside the container. You only need to mount your scenarios folder if you want to use custom scenarios from the host.

- Windows (PowerShell):
	- Script: docker_setup/setup_user_docker_env.ps1
- Linux/macOS (bash):
	- Script: docker_setup/setup_user_docker_env.sh

Both scripts will pull a prebuilt image and run the container with appropriate mounts.

#### Quick starts

- Windows (PowerShell):
	- Default (main image):
		- Open PowerShell in the repo root and run:
			- docker_setup/setup_user_docker_env.ps1
	- With a custom scenarios directory mounted (replaces the container’s /IMPACTncd_Japan/scenarios):
		- docker_setup/setup_user_docker_env.ps1 -ScenariosDir "C:\\path\\to\\your-scenarios"
	- Use a specific image tag (for example, a release tag):
		- docker_setup/setup_user_docker_env.ps1 -Tag "v1.2.3"

- Linux/macOS (bash):
	- Default (main image):
		- bash docker_setup/setup_user_docker_env.sh
	- With a custom scenarios directory mounted:
		- bash docker_setup/setup_user_docker_env.sh --scenarios-dir /path/to/your-scenarios
	- Use a specific image tag:
		- bash docker_setup/setup_user_docker_env.sh --tag v1.2.3

Notes
- Tag handling (summary):
	- main (default): pulls chriskypri/impactncdjpn:main
	- local: uses impactncdjpn:local (if you built locally)
	- any other string: pulls chriskypri/impactncdjpn:<tag>
- ScenariosDir/--scenarios-dir: mounts your scenarios as /IMPACTncd_Japan/scenarios inside the container.

### 4) Run the model inside the container

The helper scripts start a container ready to run the model. Typical entry points:
- simulate.R or simulate_mrtl_paper.R to execute simulations.
- inputs/sim_design.yaml to configure run parameters.

From the container’s shell:
- Rscript simulate.R
	- Adjust inputs/sim_design.yaml or scenarios as needed.

### 5) Outputs

Outputs are written to the location configured in inputs/sim_design.yaml (e.g., /mnt/storage_fast4/jpn/outputs). If you mount a host directory for outputs, you’ll see results appear on your host filesystem.

### 6) Stopping and re-running

- Stop the container from the Docker UI or with docker stop <container>.
- Re-run the helper script to start a new session. You can update scenario or design files between runs.

## Developer setup (Windows PowerShell): setup_dev_docker_env.ps1

This script is for developers who want a fast local dev loop with a reproducible Docker environment.

What it does
- Builds a local development image: prerequisite.impactncdjpn:local (from Dockerfile.prerequisite.IMPACTncdJPN), only when inputs change.
- Reads output_dir and synthpop_dir from your sim_design.yaml and ensures those folders exist.
- Runs the container as a non-root user (UID/GID 1000:1000) to avoid permission issues.
- Two modes:
	1) -UseVolumes: copies project/outputs/synthpop into Docker volumes for faster I/O; after exit, rsyncs results back and removes volumes.
	2) Bind mounts (default): directly mounts your folders into the container (simpler, slower on Windows/macOS).

Usage (PowerShell)
- Open PowerShell in the repo root and run:

```
docker_setup/setup_dev_docker_env.ps1
```

- Use Docker volumes (recommended on Windows/macOS):

```
docker_setup/setup_dev_docker_env.ps1 -UseVolumes
```

- Use a specific sim_design.yaml:

```
docker_setup/setup_dev_docker_env.ps1 -SimDesignYaml .\inputs\sim_design.yaml -UseVolumes
```

Inside the container
- You’ll land in /IMPACTncd_Japan. Typical commands:
	- Rscript simulate.R
	- Rscript simulate_mrtl_paper.R
- Edit inputs/sim_design.yaml or scenarios on the host, then re-run the script.

How rebuilds are detected
- The script hashes Dockerfile.prerequisite.IMPACTncdJPN, apt-packages.txt, r-packages.txt, and entrypoint.sh. If any change, it rebuilds the image, otherwise it reuses it.

Notes
- Windows execution policy: if blocked, run Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass in the same PowerShell session.
- Ensure Docker Desktop is running and docker info works.
- -UseVolumes mode copies the project into a volume (excluding dot files), runs the container, then rsyncs outputs, synthpop, and the simulation folder back to your host.
- Bind mount mode performs path conversions for Docker Desktop/WSL automatically.

Linux/macOS developers
- A bash equivalent exists: docker_setup/setup_dev_docker_env.sh with similar behavior and flags.

Troubleshooting
- Permission errors: the script runs containers as UID/GID 1000; volumes and rsync steps adjust ownership/permissions.
- Slow I/O on Windows/macOS: prefer -UseVolumes mode.
- Paths not found: ensure output_dir and synthpop_dir are valid in inputs/sim_design.yaml (absolute paths on Windows/macOS are okay; relative paths are resolved to the repo root).

## Troubleshooting

- Docker permissions (Linux): ensure your user can run docker without sudo (see Linux post-install link above).
- Proxy/corporate environments: configure Docker Desktop/Engine to use your proxy if pulls fail.
- Disk space: synthpop and outputs can be large; ensure adequate space in the configured directories.


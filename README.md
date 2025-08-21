# IMPACT<sub>NCD-Japan</sub> microsimulation

--------------------------------------------------------------------------------

IMPACT<sub>NCD-Japan</sub> is an implementation of the IMPACTncd framework, developed by Chris
Kypridemos with contributions from Peter Crowther (Melandra Ltd), Maria
Guzman-Castillo, Amandine Robert, Max Birkett, Piotr Bandosz, and Soshiro Ogata. 

Copyright (C) 2018-2025 University of Liverpool, Chris Kypridemos

IMPACT<sub>NCD-Japan</sub> is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 3 of the License, or (at your option) any later
version. This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
details. You should have received a copy of the GNU General Public License along
with this program; if not, see <http://www.gnu.org/licenses/> or write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.


## What this model is

IMPACT<sub>NCD-Japan</sub> is a population microsimulation model built on the IMPACT<sub>NCD</sub> framework. It simulates Japanese adults’ lifecourse, exposure trajectories (e.g., smoking, BMI, blood pressure), disease onset (CHD, stroke, diabetes, etc.), and mortality. It’s designed to evaluate baseline trends and policy scenarios, producing lifecourse outputs, summary tables, and plots.

Typical uses:
- Run a baseline to quantify trends in incidence, prevalence, and mortality.
- Simulate policy scenarios by changing exposure trends or disease case fatality (i.e. survival) and compare outcomes.

Outputs are written under your configured outputs directory (by default folders such as outputs/, summaries/, tables/, plots/).

## How to use IMPACT<sub>NCD-Japan</sub> 

You don’t need to install R or system dependencies locally. We provide ready-to-run images and helper scripts.

### Run with Docker (Windows, macOS, Linux)



### 1) Install Docker

- Windows (Docker Desktop): https://docs.docker.com/desktop/install/windows-install/.
- macOS (Docker Desktop): https://docs.docker.com/desktop/install/mac-install/.
- Linux (Docker Engine): https://docs.docker.com/engine/install/. Note that for systems you do not have sudo privileges the following pages might be helpful https://docs.docker.com/engine/install/linux-postinstall/ and https://docs.docker.com/engine/security/rootless/.

After installing, ensure Docker is running (Docker Desktop app on Windows/macOS; system service on Linux).

### 2) Pull and run the docker image

The images already contain the project directory /IMPACTncd_Japan inside the container. You only need to mount your scenarios folder if you want to use custom scenarios from the host.

#### If you are using PowerShell (i.e. you use a Windows PC, but not necessarily)

First download [setup_user_docker_env.ps1](https://raw.githubusercontent.com/ChristK/IMPACTncd_Japan/refs/heads/main/docker_setup/setup_user_docker_env.ps1) in a folder you choose. You may have to right-click on the file and then select `save link as` to dowload the file rather than open it. For this example I will assume you downloaded it to `C:\IMPACTncdJapan`. Then create a new folder called `scenarios` within `C:\IMPACTncdJapan` and download [simulate_scn.R](https://raw.githubusercontent.com/ChristK/IMPACTncd_Japan/refs/heads/main/scenarios/simulate_scn.R) and [sim_design_scn.yaml](https://raw.githubusercontent.com/ChristK/IMPACTncd_Japan/refs/heads/main/scenarios/sim_design_scn.yaml) to `C:\IMPACTncdJapan\scenarios`. `sim_design_scn.yaml` contains some fundamental parameters that define how the simulation runs. The file is human readable and editable in any text editor, such as Notepad or Visual Studio Code. 

Open `C:\IMPACTncdJapan\scenarios\sim_design_scn.yaml` in your favourite text editor and have a look of its structure.Find `clusternumber`, `clusternumber_export`, `output_dir` and `synthpop_dir` parameters as you will have to update them to reflect your current setup. `clusternumber` and `clusternumber_export` refer to the number of cores that you want to use for the simulation. Each core requires about 12Gb of RAM at the default settings, so even if you have i.e. 8 cores but only 32Gb of RAM, set `clusternumber` and `clusternumber_export` to 2. `output_dir` and `synthpop_dir` define where the simulation will store its outputs and some intermediate synthetic population files, respectively, and need to point to a valid folders on your host machine. For example, you can set, by overwritting the existing values:

```yaml
# some parameters at the top left unchanged
clusternumber: 2 
clusternumber_export: 2 
# some other parameters left unchanged
output_dir: C:\IMPACTncdJapan\outputs
synthpop_dir: C:\IMPACTncdJapan\synthpop
```

Remember to create the `outputs` and `synthpop` folders in `C:\IMPACTncdJapan` before running the simulation.


Now open PowerShell in `C:\IMPACTncdJapan` folder:
- Press **Win + R**, type `powershell`, and press Enter.
- In PowerShell, navigate to the folder with:
  ```powershell
  cd C:\IMPACTncdJapan
  ```
Alternatively, in File Explorer navigate to `C:\IMPACTncdJapan`, then right-click in the folder and select **Open in Terminal** (or **Open PowerShell window here** on older Windows versions).

Then ensure setup_user_docker_env.ps1 is executable. In PowerShell, “executable” means the script is allowed to run under your current execution policy.
To check, run:

```powershell
Get-ExecutionPolicy -List
```

If the policy is `Restricted`, scripts cannot run. For local scripts like setup_user_docker_env.ps1, you can temporarily allow execution just for the current session:

```powershell
Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass
.\setup_user_docker_env.ps1
```

Or you can set it permanently for your user:

```powershell
Set-ExecutionPolicy -Scope CurrentUser -ExecutionPolicy RemoteSigned
```

Now you are ready to execute the script, but before you do, let's briefly review what it does and what arguments it accepts. The script pulls and runs a Docker container for the IMPACT<sub>NCD-Japan</sub> microsimulation. It accepts four arguments:

- `-Tag`: If Tag is "main" (default), the script pulls and uses "chriskypri/impactncdjpn:main". You can pull other branches available on GitHub instead of "main", for testing.
- `-ScenariosDir`: Path to a directory containing custom scenarios to mount inside the container. This is where you can store your own scenario scripts for the model to run. For this tutorial I will assume you stored your scenarios in `C:\IMPACTncdJapan\scenarios`.
- `-SimDesignYaml`: Path to a custom sim_design file. For this tutorial you can use the one you updated earlier located at `C:\IMPACTncdJapan\scenarios\sim_design_scn.yaml`.
- `-UseVolumes`: If specified, the script will use Docker-managed volumes for enhanced I/O performance (recommended for macOS and Windows). In this mode:
      - Separate Docker volumes for the output_dir and synthpop_dir (as defined
        in the YAML file) are created and pre-populated from the local folders.
      - After the container exits, the contents of the output and synthpop volumes
        are synchronized back to the corresponding local folders using rsync.
      - All volumes are then removed.
If not specified, the script will use direct bind mounts instead of volumes (less efficient, but useful for interactive access)

Now we are ready to execute the script. 

```powershell
.\setup_user_docker_env.ps1 -Tag "main" -ScenariosDir "C:\IMPACTncdJapan\scenarios" -SimDesignYaml "C:\IMPACTncdJapan\scenarios\sim_design_scn.yaml" -UseVolumes
```

The first time you will run this script, it may take a while to pull the Docker image and set up the environment. Subsequent runs should be faster.

When the script completes, you will see the prompt again but this time you will be inside the simulation environment. The operating system of the simulation environment is always Linux Ubuntu irrespective of the operating system of your machine. You can now run a test simulation to unsure it works as expected, with:

```powershell
Rscript ./scenarios/simulate_scn.R
```

When the simulation finishes, hopefully without errors, you will see the message "Simulation has finished!". You can then type `exit` to exit the simulation environment. You can inspect the outputs of the model, saved in `C:\IMPACTncdJapan\outputs`, and the synthetic population files in `C:\IMPACTncdJapan\synthpop`.


#### If you are on Linux or macOS

First download [setup_user_docker_env.sh](https://raw.githubusercontent.com/ChristK/IMPACTncd_Japan/refs/heads/main/docker_setup/setup_user_docker_env.sh) in a folder you choose. You may have to right-click on the file and then select `save link as` to dowload the file rather than open it.For this example I will assume you downloaded it to `~/IMPACTncdJapan`. Note that `~` denotes your user home folder. For instance, if your username is `chris`:
  - On Linux, `~` expands to `/home/chris`
  - On macOS, `~` expands to `/Users/chris`

Then create a new folder called `scenarios` within `~/IMPACTncdJapan` and download [simulate_scn.R](https://raw.githubusercontent.com/ChristK/IMPACTncd_Japan/refs/heads/main/scenarios/simulate_scn.R) and [sim_design_scn.yaml](https://raw.githubusercontent.com/ChristK/IMPACTncd_Japan/refs/heads/main/scenarios/sim_design_scn.yaml) to `~/IMPACTncdJapan/scenarios`. `sim_design_scn.yaml` contains some fundamental parameters that define how the simulation runs. The file is human readable and editable in any text editor, such as gedit or Visual Studio Code.

Open `~/IMPACTncdJapan/scenarios/sim_design_scn.yaml` in your favourite text editor and have a look of its structure.Find `clusternumber`, `clusternumber_export`, `output_dir` and `synthpop_dir` parameters as you will have to update them to reflect your current setup. `clusternumber` and `clusternumber_export` refer to the number of cores that you want to use for the simulation. Each core requires about 12Gb of RAM at the default settings, so even if you have i.e. 8 cores but only 32Gb of RAM, set `clusternumber` and `clusternumber_export` to 2. `output_dir` and `synthpop_dir` define where the simulation will store its outputs and some intermediate synthetic population files, respectively, and need to point to a valid folders on your host machine. For example, you can set:

```yaml
# some parameters at the top left unchanged
clusternumber: 2 
clusternumber_export: 2 
# some other parameters left unchanged
output_dir: ~/IMPACTncdJapan/outputs
synthpop_dir: ~/IMPACTncdJapan/synthpop
```

Now open a Terminal and navigate to the `~/IMPACTncdJapan` directory:

```bash
cd ~/IMPACTncdJapan
```
Ensure `setup_user_docker_env.sh` is executable with:

```bash
chmod +x setup_user_docker_env.sh
```

If you are on macOS, `coreutils` is required for some of the script's functionality. You can install it using Homebrew:

```bash
brew install coreutils
```

If you don’t already have Homebrew installed, you can install it by pasting the following in a macOS Terminal:

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

Now you are ready to execute the script, but before you do, let's briefly review what it does and what arguments it accepts. The script pulls and runs a Docker container for the IMPACT<sub>NCD-Japan</sub> microsimulation. It accepts four arguments:

- `Tag`: If Tag is "main" (default), the script pulls and uses "chriskypri/impactncdjpn:main". You can pull other branches available on GitHub instead of "main", for testing.
- `-ScenariosDir`: Path to a directory containing custom scenarios to mount inside the container. This is where you can store your own scenario scripts for the model to run. For this tutorial I will assume you stored your scenarios in `C:\IMPACTncdJapan\scenarios`.
- `-SimDesignYaml`: Path to a custom sim_design file. For this tutorial you can use the one you updated earlier located at `C:\IMPACTncdJapan\scenarios\sim_design_scn.yaml`.
- `-UseVolumes`: If specified, the script will use Docker-managed volumes for enhanced I/O performance (recommended for macOS and Windows). In this mode:
      - Separate Docker volumes for the output_dir and synthpop_dir (as defined
        in the YAML file) are created and pre-populated from the local folders.
      - After the container exits, the contents of the output and synthpop volumes
        are synchronized back to the corresponding local folders using rsync.
      - All volumes are then removed.
If not specified, the script will use direct bind mounts instead of volumes (less efficient, but useful for interactive access)

Now we are ready to execute the script. 

```bash
./setup_user_docker_env.sh -Tag main -ScenariosDir ~/IMPACTncdJapan/scenarios -SimDesignYaml ~/IMPACTncdJapan/scenarios/sim_design_scn.yaml --UseVolumes
```

The first time you will run this script, it may take a while to pull the Docker image and set up the environment. Subsequent runs should be faster.

When the script completes, you will see the prompt again but this time you will be inside the simulation environment. The operating system of the simulation environment is always Linux Ubuntu irrespective of the operating system of your machine. You can now run a test simulation to unsure it works as expected, with:

```powershell
Rscript ./scenarios/simulate_scn.R
```

When the simulation finishes, hopefully without errors, you will see the message "Simulation has finished!". You can then type `exit` to exit the simulation environment. You can inspect the outputs of the model, saved in `~/IMPACTncdJapan/outputs`, and the synthetic population files in `~/IMPACTncdJapan/synthpop`.

### 3) Re-running the model

You can re-run the `setup_user_docker_env` script to start a new session. You can update scenario or design files between runs.

## Developer setup

If you are a developer or poweruser, you may want to set up a more flexible development environment. You can first clone or download this repository to your machine.

### (Windows PowerShell): setup_dev_docker_env.ps1

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

```powershell
docker_setup/setup_dev_docker_env.ps1
```

- Use Docker volumes (recommended on Windows/macOS):

```powershell
docker_setup/setup_dev_docker_env.ps1 -UseVolumes
```

- Use a specific sim_design.yaml:
```powershell
docker_setup/setup_dev_docker_env.ps1 -SimDesignYaml .\inputs\sim_design_clbr.yaml -UseVolumes
```

Inside the container
- You’ll land in /IMPACTncd_Japan. Typical commands:
	- Rscript simulate.R
	- Rscript /scenarios/simulate_scn.R
- Edit inputs/sim_design.yaml or scenarios on the host, then re-run the script.

How rebuilds are detected
- The script hashes Dockerfile.prerequisite.IMPACTncdJPN, apt-packages.txt, r-packages.txt, and entrypoint.sh. If any change, it rebuilds the image, otherwise it reuses it.

Notes
- Windows execution policy: if blocked, run Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass in the same PowerShell session.
- Ensure Docker Desktop is running and docker info works.
- -UseVolumes mode copies the project into a volume (excluding dot files), runs the container, then rsyncs outputs, synthpop, and the simulation folder back to your host.
- Bind mount mode performs path conversions for Docker Desktop/WSL automatically.

### Linux/macOS developers
- A bash equivalent exists: docker_setup/setup_dev_docker_env.sh with similar behavior and flags.

Troubleshooting
- Permission errors: the script runs containers as UID/GID 1000; volumes and rsync steps adjust ownership/permissions.
- Slow I/O on Windows/macOS: prefer -UseVolumes mode.
- Paths not found: ensure output_dir and synthpop_dir are valid in inputs/sim_design.yaml (absolute paths on Windows/macOS are okay; relative paths are resolved to the repo root).

## Troubleshooting

- Docker permissions (Linux): ensure your user can run docker without sudo (see Linux post-install link above).
- Proxy/corporate environments: configure Docker Desktop/Engine to use your proxy if pulls fail.
- Disk space: synthpop and outputs can be large; ensure adequate space in the configured directories.

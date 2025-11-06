# Docker Setup Guide

## Docker Setup Guide

Docker provides the most reliable way to run IMPACTncd-Japan across
different operating systems and ensures reproducible results.

### Docker Installation

#### Windows

1.  Download [Docker Desktop for
    Windows](https://docs.docker.com/desktop/install/windows-install/)
2.  Run the installer and follow the setup wizard
3.  Restart your computer when prompted
4.  Launch Docker Desktop and complete the initial setup

#### macOS

1.  Download [Docker Desktop for
    Mac](https://docs.docker.com/desktop/install/mac-install/)
2.  Drag Docker to your Applications folder
3.  Launch Docker Desktop and complete the initial setup

#### Linux

Install Docker Engine using your distribution’s package manager:

``` bash
# Ubuntu/Debian
sudo apt-get update
sudo apt-get install docker-ce docker-ce-cli containerd.io

# CentOS/RHEL/Fedora
sudo yum install docker-ce docker-ce-cli containerd.io

# Enable and start Docker service
sudo systemctl enable docker
sudo systemctl start docker

# Add user to docker group (optional, avoids need for sudo)
sudo usermod -aG docker $USER
```

### User Setup (Recommended)

For most users who want to run simulations without modifying the model
code.

#### 1. Download Setup Script

**Windows (PowerShell):**

``` powershell
# Create working directory
New-Item -ItemType Directory -Path C:\IMPACTncdJapan -Force
Set-Location C:\IMPACTncdJapan

# Download setup script
Invoke-WebRequest -Uri "https://raw.githubusercontent.com/ChristK/IMPACTncd_Japan/refs/heads/main/docker_setup/setup_user_docker_env.ps1" -OutFile "setup_user_docker_env.ps1"
```

**Linux/macOS:**

``` bash
# Create working directory
mkdir -p ~/IMPACTncdJapan
cd ~/IMPACTncdJapan

# Download setup script
curl -O https://raw.githubusercontent.com/ChristK/IMPACTncd_Japan/refs/heads/main/docker_setup/setup_user_docker_env.sh
chmod +x setup_user_docker_env.sh
```

#### 2. Create Scenarios Directory

``` bash
# Create scenarios directory and download example files
mkdir scenarios

# Download example scenario files
curl -o scenarios/simulate_scn.R https://raw.githubusercontent.com/ChristK/IMPACTncd_Japan/refs/heads/main/scenarios/simulate_scn.R
curl -o scenarios/sim_design_scn.yaml https://raw.githubusercontent.com/ChristK/IMPACTncd_Japan/refs/heads/main/scenarios/sim_design_scn.yaml
```

#### 3. Configure Simulation

Edit `scenarios/sim_design_scn.yaml`:

``` yaml
# Adjust these parameters for your system
clusternumber: 2              # Number of CPU cores (each needs ~12GB RAM)
clusternumber_export: 2       # Cores for export processing
output_dir: /path/to/outputs  # Where to save results
synthpop_dir: /path/to/synthpop # Where to save intermediate files
```

**Important**: Create the output directories before running:

``` bash
# Windows
mkdir C:\IMPACTncdJapan\outputs, C:\IMPACTncdJapan\synthpop

# Linux/macOS  
mkdir -p ~/IMPACTncdJapan/outputs ~/IMPACTncdJapan/synthpop
```

#### 4. Run Simulation

**Windows (PowerShell):**

``` powershell
# Allow script execution (if needed)
Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass

# Run simulation
.\setup_user_docker_env.ps1 -Tag "main" -ScenariosDir "C:\IMPACTncdJapan\scenarios" -SimDesignYaml "C:\IMPACTncdJapan\scenarios\sim_design_scn.yaml" -UseVolumes
```

**Linux/macOS:**

``` bash
./setup_user_docker_env.sh -Tag main -ScenariosDir ~/IMPACTncdJapan/scenarios -SimDesignYaml ~/IMPACTncdJapan/scenarios/sim_design_scn.yaml --UseVolumes
```

#### 5. Inside the Container

Once the container starts, you’ll have a shell prompt. Run your
simulation:

``` bash
# Run the example scenario
Rscript ./scenarios/simulate_scn.R

# Exit when done
exit
```

### Developer Setup

For developers who want to modify the model code or contribute to the
project.

#### 1. Clone Repository

``` bash
git clone https://github.com/ChristK/IMPACTncd_Japan.git
cd IMPACTncd_Japan
```

#### 2. Run Development Container

**Windows (PowerShell):**

``` powershell
# Run with volume mounting for better performance
.\docker_setup\setup_dev_docker_env.ps1 -UseVolumes

# Or with direct bind mounts (simpler but slower)
.\docker_setup\setup_dev_docker_env.ps1
```

**Linux/macOS:**

``` bash
# Run with volume mounting
./docker_setup/setup_dev_docker_env.sh -UseVolumes

# Or with bind mounts
./docker_setup/setup_dev_docker_env.sh
```

#### 3. Development Workflow

Inside the development container:

``` bash
# Install package in development mode
R CMD INSTALL Rpackage/IMPACTncd_Japan_model_pkg

# Run tests
Rscript -e "devtools::test('Rpackage/IMPACTncd_Japan_model_pkg')"

# Run simulations
Rscript simulate.R

# Generate documentation
Rscript -e "devtools::document('Rpackage/IMPACTncd_Japan_model_pkg')"
```

### Docker Configuration Options

#### Script Parameters

Both setup scripts accept these parameters:

- `-Tag`: Docker image tag (default: “main”)
- `-ScenariosDir`: Path to custom scenarios directory
- `-SimDesignYaml`: Path to simulation design YAML file
- `-UseVolumes`: Use Docker volumes for better I/O performance

#### Performance Modes

**Bind Mounts (Default):** - Direct mounting of host directories -
Changes immediately visible on host - Slower I/O on Windows/macOS -
Better for interactive development

**Volume Mode (`-UseVolumes`):** - Copies data to Docker volumes -
Faster I/O performance - Syncs results back to host after completion -
Better for production runs

### Hardware Requirements

#### Minimum Requirements

- **CPU**: 2+ cores
- **RAM**: 16GB (allows 1 simulation core)
- **Storage**: 20GB free space
- **Docker**: 4GB memory limit for Docker Desktop

#### Recommended Configuration

- **CPU**: 4-8 cores
- **RAM**: 32-64GB
- **Storage**: 100GB+ SSD
- **Docker**: 8GB+ memory limit

#### Resource Planning

| Cores | RAM Needed | Population Size | Typical Runtime |
|-------|------------|-----------------|-----------------|
| 1     | 12GB       | 100K            | 2-4 hours       |
| 2     | 24GB       | 200K            | 2-4 hours       |
| 4     | 48GB       | 400K            | 2-4 hours       |
| 8     | 96GB       | 800K            | 2-4 hours       |

### Troubleshooting

#### Common Issues

##### Docker not starting

``` bash
# Check Docker status
docker info

# Restart Docker service (Linux)
sudo systemctl restart docker

# Restart Docker Desktop (Windows/macOS)
# Use the system tray/menu bar icon
```

##### Permission errors (Linux)

``` bash
# Add user to docker group
sudo usermod -aG docker $USER

# Log out and back in, or run:
newgrp docker
```

##### Memory issues

``` yaml
# Reduce cores in sim_design.yaml
clusternumber: 1
clusternumber_export: 1

# Or reduce population size
pop_size: 50000
```

##### Network issues

``` bash
# Test Docker Hub connectivity
docker pull hello-world

# Configure proxy if needed (corporate networks)
# Edit ~/.docker/config.json
```

#### Performance Optimization

##### Windows/macOS

- Enable WSL 2 backend (Windows)
- Increase Docker Desktop memory limit
- Use `-UseVolumes` mode for better I/O
- Place output directories on fast storage

##### Linux

- Use native Docker (not Docker Desktop)
- Configure storage driver for performance
- Use tmpfs for temporary files:

``` yaml
# In sim_design.yaml
temp_dir: /tmp/impactncdjapan
```

#### Advanced Configuration

##### Custom Docker Images

``` bash
# Build custom image with additional packages
docker build -t my-impactncdjapan -f docker_setup/Dockerfile.custom .

# Use custom image
./setup_user_docker_env.sh -Tag my-impactncdjapan
```

##### Cluster/HPC Usage

``` bash
# Convert Docker image to Singularity
singularity pull impactncdjapan.sif docker://chriskypri/impactncdjpn:main

# Run on cluster
singularity exec impactncdjapan.sif Rscript simulate.R
```

### Security Considerations

#### Data Protection

- Docker containers run with limited privileges
- Data directories are mounted read-write only as needed
- No network services exposed by default

#### User Permissions

- Development containers run as non-root user (UID 1000)
- Output files maintain appropriate permissions
- No sudo access within containers

#### Network Security

- Containers only access necessary external resources
- No incoming network connections
- Docker images pulled from verified sources

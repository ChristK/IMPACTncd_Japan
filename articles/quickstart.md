# Quick Start Guide

## Getting Started with IMPACTncd-Japan

IMPACTncd-Japan is a population microsimulation model built on the
IMPACTncd framework. It simulates Japanese adults’ lifecourse, exposure
trajectories (e.g., smoking, BMI, blood pressure), disease onset (CHD,
stroke, diabetes, etc.), and mortality.

### What this model does

- **Baseline analysis**: Quantify trends in incidence, prevalence, and
  mortality
- **Policy evaluation**: Simulate scenarios by changing exposure trends
  or disease case fatality
- **Lifecourse modeling**: Track individuals through their entire health
  journey
- **Population health impact**: Assess interventions at the population
  level

### Quick Start with Docker

The fastest way to get started is using our pre-built Docker images:

#### 1. Install Docker

- **Windows/macOS**: Install [Docker
  Desktop](https://www.docker.com/products/docker-desktop)
- **Linux**: Install [Docker
  Engine](https://docs.docker.com/engine/install/)

#### 2. Download the setup script

Download the appropriate script for your system:

- **Windows PowerShell**:
  [setup_user_docker_env.ps1](https://raw.githubusercontent.com/ChristK/IMPACTncd_Japan/refs/heads/main/docker_setup/setup_user_docker_env.ps1)
- **Linux/macOS**:
  [setup_user_docker_env.sh](https://raw.githubusercontent.com/ChristK/IMPACTncd_Japan/refs/heads/main/docker_setup/setup_user_docker_env.sh)

#### 3. Create your workspace

``` bash
# Create a directory for your work
mkdir ~/IMPACTncdJapan
cd ~/IMPACTncdJapan

# Create scenarios directory
mkdir scenarios

# Download example scenario files
curl -o scenarios/simulate_scn.R https://raw.githubusercontent.com/ChristK/IMPACTncd_Japan/refs/heads/main/scenarios/simulate_scn.R
curl -o scenarios/sim_design_scn.yaml https://raw.githubusercontent.com/ChristK/IMPACTncd_Japan/refs/heads/main/scenarios/sim_design_scn.yaml
```

#### 4. Configure your simulation

Edit `scenarios/sim_design_scn.yaml` to set:

- `clusternumber`: Number of CPU cores to use (each needs ~12GB RAM)
- `output_dir`: Where to save results
- `synthpop_dir`: Where to save intermediate files

#### 5. Run your first simulation

``` bash
# Make the script executable (Linux/macOS)
chmod +x setup_user_docker_env.sh

# Run the container
./setup_user_docker_env.sh -Tag main -ScenariosDir ~/IMPACTncdJapan/scenarios -SimDesignYaml ~/IMPACTncdJapan/scenarios/sim_design_scn.yaml --UseVolumes

# Inside the container, run a test simulation
Rscript ./scenarios/simulate_scn.R
```

### What happens during a simulation

1.  **Synthetic population generation**: Creates a representative
    population
2.  **Lifecourse simulation**: Models individual trajectories over time
3.  **Disease progression**: Simulates onset and progression of diseases
4.  **Policy scenarios**: Applies interventions and measures impact
5.  **Results export**: Generates tables, plots, and summary statistics

### Output structure

Results are saved in your configured output directory:

- `summaries/`: Statistical summaries of results
- `tables/`: Formatted tables for publication
- `plots/`: Visualization of trends and comparisons
- `lifecourse/`: Individual-level trajectories
- `logs/`: Simulation logs and diagnostics

### Next steps

- Read the [Installation
  Guide](https://christk.github.io/IMPACTncd_Japan/articles/installation.md)
  for local development setup
- Learn about [Running
  Simulations](https://christk.github.io/IMPACTncd_Japan/articles/running-simulations.md)
  in detail
- Explore [Scenario
  Analysis](https://christk.github.io/IMPACTncd_Japan/articles/scenario-analysis.md)
  for policy evaluation

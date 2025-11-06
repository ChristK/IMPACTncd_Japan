# IMPACTncd-Japan ![](reference/figures/logo.png)

IMPACTncd-Japan is a population microsimulation model built on the
IMPACTncd framework. It simulates Japanese adults’ lifecourse, exposure
trajectories (e.g., smoking, BMI, blood pressure), disease onset (CHD,
stroke, diabetes, etc.), and mortality.

## What this model does

- **Baseline analysis**: Quantify trends in incidence, prevalence, and
  mortality
- **Policy evaluation**: Simulate scenarios by changing exposure trends
  or disease case fatality
- **Lifecourse modeling**: Track individuals through their entire health
  journey
- **Population health impact**: Assess interventions at the population
  level

## Installation

### Docker (Recommended)

The fastest way to get started is using our pre-built Docker images:

``` bash
# Download setup script
curl -O https://raw.githubusercontent.com/ChristK/IMPACTncd_Japan/refs/heads/main/docker_setup/setup_user_docker_env.sh
chmod +x setup_user_docker_env.sh

# Run simulation
./setup_user_docker_env.sh -Tag main -ScenariosDir ./scenarios -SimDesignYaml ./scenarios/sim_design.yaml --UseVolumes
```

### Local Installation

For development or if you prefer local installation:

``` r
# Install dependencies
install.packages(c("Rcpp", "data.table", "fst", "dqrng", "gamlss", 
                   "cowplot", "mc2d", "digest", "R6", "yaml", "foreach", 
                   "BH", "igraph", "parallelly", "parallel"))

# Install CKutils
devtools::install_github("ChristK/CKutils")

# Install IMPACTncdJapan
devtools::install_github("ChristK/IMPACTncd_Japan", 
                         subdir = "Rpackage/IMPACTncd_Japan_model_pkg")
```

## Quick Start

``` r
library(IMPACTncdJapan)

# Create simulation design
design <- Design$new()
design$read_yaml("./inputs/sim_design.yaml")

# Run simulation
sim <- Simulation$new(design)
sim$run()

# Export results
sim$export_summaries()
```

## Documentation

- **[Quick Start
  Guide](https://christk.github.io/IMPACTncd_Japan/articles/quickstart.html)**:
  Get up and running quickly
- **[Installation
  Guide](https://christk.github.io/IMPACTncd_Japan/articles/installation.html)**:
  Detailed installation instructions
- **[Docker
  Setup](https://christk.github.io/IMPACTncd_Japan/articles/docker-setup.html)**:
  Using Docker containers
- **[Running
  Simulations](https://christk.github.io/IMPACTncd_Japan/articles/running-simulations.html)**:
  Detailed simulation guide
- **[Scenario
  Analysis](https://christk.github.io/IMPACTncd_Japan/articles/scenario-analysis.html)**:
  Policy evaluation framework

## Key Features

### Core Classes

- **`Design`**: Simulation configuration and parameters
- **`Simulation`**: Main simulation engine  
- **`SynthPop`**: Synthetic population generation
- **`Disease`**: Disease progression modeling
- **`Exposure`**: Risk factor trajectory modeling

### Disease Models

- Coronary Heart Disease (CHD)
- Stroke
- Diabetes  
- COPD
- Lung Cancer

### Risk Factors

- Body Mass Index (BMI)
- Smoking
- Blood Pressure
- Cholesterol
- Physical Activity

## System Requirements

- **R**: Version 4.1.0 or higher
- **Memory**: Minimum 16GB RAM (32GB+ recommended)
- **Storage**: 50GB+ free space for outputs
- **Compiler**: C++17 compatible

Each simulation core requires approximately 12GB of RAM.

## Performance

| Population Size | Cores | RAM       | Time Estimate |
|-----------------|-------|-----------|---------------|
| 100K            | 1     | 12GB      | 2-4 hours     |
| 1M              | 4-8   | 48-96GB   | 4-8 hours     |
| 10M             | 16-32 | 192-384GB | 8-24 hours    |

## Getting Help

- **Issues**: Report bugs on [GitHub
  Issues](https://github.com/ChristK/IMPACTncd_Japan/issues)
- **Documentation**: Visit our
  [website](https://christk.github.io/IMPACTncd_Japan/)
- **Discussions**: Use [GitHub
  Discussions](https://github.com/ChristK/IMPACTncd_Japan/discussions)

## Contributing

We welcome contributions! Please see our [Contributing
Guide](https://christk.github.io/IMPACTncd_Japan/CONTRIBUTING.md) for
details.

## License

This project is licensed under the GNU General Public License v3.0 - see
the [LICENSE](https://christk.github.io/IMPACTncd_Japan/LICENSE) file
for details.

## Citation

If you use IMPACTncd-Japan in your research, please cite:

    Kypridemos, C., Crowther, P., Guzman-Castillo, M., Robert, A., Birkett, M.,
    Bandosz, P., & Ogata, S. (2025). IMPACTncd-Japan: A microsimulation model
    for population health impact assessment in Japan.
    GitHub: https://github.com/ChristK/IMPACTncd_Japan

## Contributors

- **Chris Kypridemos** (University of Liverpool) - Lead developer
- **Peter Crowther** (Melandra Ltd) - Core infrastructure  
- **Maria Guzman-Castillo** - Disease modeling
- **Amandine Robert** - Validation and testing
- **Max Birkett** - Documentation
- **Piotr Bandosz** - Model design
- **Soshiro Ogata** - Japan-specific calibration

------------------------------------------------------------------------

Copyright (C) 2018-2025 University of Liverpool, Chris Kypridemos

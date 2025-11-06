# Running Simulations

## Running Simulations

This guide covers how to run simulations with IMPACTncd-Japan, from
basic configurations to advanced scenarios.

### Simulation Overview

A typical simulation involves:

1.  **Configuration**: Set parameters via YAML files
2.  **Population synthesis**: Generate synthetic individuals
3.  **Lifecourse modeling**: Simulate health trajectories
4.  **Disease progression**: Model disease onset and progression  
5.  **Output generation**: Create tables, plots, and summaries

### Configuration Files

#### Main Configuration (`sim_design.yaml`)

The primary configuration file controls all aspects of the simulation:

``` yaml
# Basic simulation parameters
mc: 10                    # Number of Monte Carlo iterations
years: [2011, 2040]      # Simulation time period
init_year: 2011          # Baseline year
clusternumber: 4         # CPU cores for simulation
clusternumber_export: 2  # CPU cores for output processing

# Population parameters
pop_size: 100000         # Synthetic population size
age_range: [20, 100]     # Age range to simulate

# Output directories
output_dir: ./outputs
synthpop_dir: ./synthpop
logs_dir: ./logs

# Scenario parameters
scenario_name: "baseline"
scenario_description: "Baseline trends with no interventions"

# Disease modules to include
diseases:
  - CHD
  - Stroke  
  - Diabetes
  - COPD
  - Lung_cancer

# Risk factor modules
exposures:
  - BMI
  - Smoking
  - Blood_pressure
  - Cholesterol
  - Physical_activity
```

#### Advanced Configuration Options

``` yaml
# Calibration settings
calibration:
  enabled: true
  target_year: 2019
  tolerance: 0.05
  max_iterations: 100

# Scenario interventions
interventions:
  smoking:
    type: "trend_change"
    start_year: 2025
    target_reduction: 0.50  # 50% reduction by 2040
    
  bmi:
    type: "policy_shock"
    implementation_year: 2030
    effect_size: -1.5       # Reduce mean BMI by 1.5 kg/m²

# Output customization
outputs:
  tables:
    age_groups: [20, 30, 40, 50, 60, 70, 80]
    summary_years: [2020, 2025, 2030, 2035, 2040]
  
  plots:
    generate_trends: true
    generate_comparisons: true
    format: "png"
    dpi: 300
```

### Running Simulations

#### Basic Simulation

``` r
# Load the package
library(IMPACTncdJapan)

# Create simulation design from YAML
design <- Design$new()
design$read_yaml("./inputs/sim_design.yaml")

# Create and run simulation
sim <- Simulation$new(design)
sim$run()

# Export results
sim$export_summaries()
```

#### Simulation with Custom Parameters

``` r
# Create design with custom parameters
design <- Design$new()

# Override specific parameters
design$sim_prm$mc <- 5
design$sim_prm$clusternumber <- 2
design$sim_prm$pop_size <- 50000

# Set scenario details
design$sim_prm$scenario_name <- "custom_test"
design$sim_prm$years <- c(2015, 2035)

# Run simulation
sim <- Simulation$new(design)
sim$run()
```

#### Batch Processing Multiple Scenarios

``` r
# Define multiple scenarios
scenarios <- list(
  baseline = list(
    name = "baseline",
    smoking_trend = 0,
    bmi_intervention = FALSE
  ),
  
  smoking_reduction = list(
    name = "smoking_50pct_reduction", 
    smoking_trend = -0.5,
    bmi_intervention = FALSE
  ),
  
  combined_intervention = list(
    name = "smoking_bmi_combined",
    smoking_trend = -0.5,
    bmi_intervention = TRUE
  )
)

# Run each scenario
results <- list()
for (scenario in scenarios) {
  design <- Design$new()
  design$read_yaml("./inputs/sim_design_base.yaml")
  
  # Apply scenario-specific parameters
  design$sim_prm$scenario_name <- scenario$name
  # ... apply other scenario parameters
  
  sim <- Simulation$new(design)
  sim$run()
  
  results[[scenario$name]] <- sim$get_summary()
}
```

### Monitoring Progress

#### Real-time Monitoring

``` r
# Enable progress tracking
design$sim_prm$verbose <- TRUE
design$sim_prm$progress_freq <- 1000  # Report every 1000 individuals

# Monitor memory usage
design$sim_prm$memory_monitoring <- TRUE
```

#### Log Analysis

``` bash
# Monitor simulation progress
tail -f ./logs/simulation_log.txt

# Check for errors
grep "ERROR" ./logs/*.txt

# Monitor memory usage
grep "Memory" ./logs/*.txt
```

### Parallel Processing

#### Multi-core Configuration

``` yaml
# Optimal settings for different hardware
# 16GB RAM system:
clusternumber: 1
clusternumber_export: 1

# 32GB RAM system:
clusternumber: 2  
clusternumber_export: 1

# 64GB+ RAM system:
clusternumber: 4
clusternumber_export: 2
```

#### Cluster/HPC Environments

``` r
# For SLURM clusters
library(parallelly)
library(future)

# Set up parallel backend
cl <- parallelly::makeClusterPSOCK(
  workers = as.numeric(Sys.getenv("SLURM_NTASKS", "1")),
  rshcmd = "srun"
)

# Configure simulation for cluster
design$sim_prm$clusternumber <- length(cl)
```

### Output Management

#### Directory Structure

Simulations create the following output structure:

    outputs/
    ├── summaries/           # Aggregated statistics
    │   ├── disease_summary.csv
    │   ├── exposure_summary.csv
    │   └── mortality_summary.csv
    ├── tables/             # Formatted tables
    │   ├── incidence_by_age.csv
    │   ├── prevalence_trends.csv
    │   └── policy_impact.csv
    ├── plots/              # Visualizations
    │   ├── disease_trends.png
    │   ├── exposure_patterns.png
    │   └── scenario_comparison.png
    ├── lifecourse/         # Individual trajectories
    │   └── mc=1/
    │       ├── individuals_1.fst
    │       └── ...
    └── logs/               # Simulation logs
        ├── simulation_log.txt
        └── error_log.txt

#### Customizing Outputs

``` r
# Customize output tables
design$tables_prm$age_breaks <- c(20, 40, 60, 80, 100)
design$tables_prm$summary_years <- c(2020, 2030, 2040)

# Control plot generation
design$plots_prm$generate_disease_plots <- TRUE
design$plots_prm$generate_exposure_plots <- TRUE
design$plots_prm$plot_format <- "png"
design$plots_prm$plot_resolution <- 300
```

### Performance Optimization

#### Memory Management

``` r
# Reduce memory usage
design$sim_prm$store_full_lifecourse <- FALSE  # Don't save all trajectories
design$sim_prm$summary_only <- TRUE            # Generate summaries only

# Garbage collection frequency
design$sim_prm$gc_freq <- 5000  # Run garbage collection every 5000 individuals
```

#### I/O Optimization

``` r
# Use faster file formats
design$export_prm$use_fst <- TRUE        # Use FST for large datasets
design$export_prm$compression <- 50      # Compress outputs

# Batch output writing
design$export_prm$batch_size <- 10000    # Write outputs in batches
```

### Troubleshooting

#### Common Issues

##### Out of Memory

``` r
# Reduce population size
design$sim_prm$pop_size <- 50000

# Use fewer cores
design$sim_prm$clusternumber <- 1

# Disable full lifecourse storage
design$sim_prm$store_full_lifecourse <- FALSE
```

##### Slow Performance

``` r
# Optimize for your hardware
design$sim_prm$clusternumber <- parallel::detectCores() - 1

# Reduce I/O frequency
design$export_prm$batch_size <- 20000

# Use ramdisk for temporary files (Linux/macOS)
design$sim_prm$temp_dir <- "/tmp/impactncdjapan"
```

##### Calibration Issues

``` r
# Increase tolerance
design$calibration_prm$tolerance <- 0.10

# Extend iteration limit
design$calibration_prm$max_iterations <- 200

# Check input data quality
design$validate_inputs()
```

#### Debugging

``` r
# Enable detailed logging
design$sim_prm$debug_mode <- TRUE

# Save intermediate results
design$sim_prm$save_checkpoints <- TRUE
design$sim_prm$checkpoint_freq <- 1000

# Profile performance
Rprof("simulation_profile.out")
sim$run()
Rprof(NULL)
```

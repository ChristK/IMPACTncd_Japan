# Model Architecture

## Model Architecture

This document describes the technical architecture of IMPACTncd-Japan,
including the underlying frameworks, data structures, and computational
approaches.

### Conceptual Framework

IMPACTncd-Japan is built on a **microsimulation** approach where:

1.  **Individual-level modeling**: Each person in the synthetic
    population is tracked separately
2.  **Discrete-time simulation**: Population evolves in annual time
    steps
3.  **Stochastic processes**: Random variation incorporated through
    Monte Carlo methods
4.  **Lifecourse perspective**: Tracks individuals from young adulthood
    to death

### Core Components

#### 1. Synthetic Population Generation

The `SynthPop` class creates a representative population using:

``` r
# Population synthesis process
SynthPop$new(design) %>%
  generate_demographics() %>%      # Age, sex distribution
  assign_risk_factors() %>%       # Initial BMI, smoking, etc.
  calibrate_prevalence() %>%      # Match observed disease prevalence
  validate_population()           # Quality checks
```

**Key features:** - Marginal distribution matching for demographics -
Copula-based correlation structure for risk factors - Iterative
proportional fitting for calibration - Cross-validation against survey
data

#### 2. Disease Progression Models

Each disease follows a similar pattern implemented in the `Disease`
class:

``` r
# Generic disease progression
Disease$new("CHD") %>%
  set_incidence_model(model_type = "competing_risks") %>%
  set_case_fatality_model(model_type = "survival") %>%
  set_prevalence_effects() %>%
  calibrate_to_targets()
```

**Disease-specific models:**

- **CHD**: Competing risks with stroke, case-fatality based on Monica
  data
- **Stroke**: Separate models for ischemic and hemorrhagic
- **Diabetes**: Type 2 diabetes with complications modeling
- **COPD**: GOLD stage progression
- **Lung Cancer**: Histology-specific survival models

#### 3. Risk Factor Trajectories

The `Exposure` class models how risk factors change over time:

``` r
# Risk factor modeling
Exposure$new("BMI") %>%
  set_trajectory_model("random_walk") %>%
  add_age_effects() %>%
  add_cohort_effects() %>%
  add_period_effects() %>%
  calibrate_trends()
```

**Modeling approaches:** - **BMI**: Age-period-cohort models with
drift - **Smoking**: Discrete state transitions (never/current/former) -
**Blood pressure**: Continuous with medication effects -
**Cholesterol**: Linked to BMI and medication use - **Physical
activity**: Ordered categorical with transitions

### Technical Implementation

#### R6 Class System

IMPACTncd-Japan uses R6 for object-oriented programming:

``` r
# Example class structure
Design <- R6::R6Class("Design",
  public = list(
    # Public methods and fields
    initialize = function() { ... },
    read_yaml = function(file) { ... },
    validate = function() { ... }
  ),
  
  private = list(
    # Private methods and data
    .validate_parameters = function() { ... },
    .data = NULL
  ),
  
  active = list(
    # Active bindings (computed properties)
    years = function() private$.data$years
  )
)
```

**Benefits:** - Encapsulation of complex state - Method chaining for
fluent APIs - Memory efficiency through reference semantics - Easy
extension and inheritance

#### C++ Backend

Performance-critical components implemented in C++ using Rcpp:

``` cpp
// Example C++ function
// [[Rcpp::export]]
NumericVector simulate_trajectory(NumericVector initial_values,
                                 NumericMatrix transition_probs,
                                 int n_periods) {
  // High-performance simulation code
  return trajectory;
}
```

**C++ components:** - Random number generation (dqrng) - Matrix
operations for large populations - Probability calculations - Disease
progression algorithms

#### Parallel Processing

Multi-core simulation using the `parallel` package:

``` r
# Parallel execution strategy
cluster <- makeCluster(design$sim_prm$clusternumber)
clusterEvalQ(cluster, library(IMPACTncdJapan))

# Distribute population chunks across cores
population_chunks <- split_population(synthpop, n_cores)
results <- parLapply(cluster, population_chunks, simulate_chunk)

# Combine results
final_results <- combine_chunk_results(results)
stopCluster(cluster)
```

**Parallelization strategy:** - Population-level parallelism (split
individuals across cores) - Scenario-level parallelism (multiple
scenarios simultaneously) - Monte Carlo parallelism (multiple runs in
parallel)

### Data Structures

#### Population Data

Stored as `data.table` for efficiency:

``` r
# Example population structure
population <- data.table(
  pid = 1:100000,           # Person ID
  age = sample(20:100, 100000, replace = TRUE),
  sex = sample(0:1, 100000, replace = TRUE),
  bmi = rnorm(100000, 25, 4),
  smoking = sample(0:2, 100000, replace = TRUE),
  # ... other variables
  alive = rep(TRUE, 100000),
  year_death = rep(NA_integer_, 100000)
)
```

#### Simulation State

Tracked through multiple data structures:

``` r
# Simulation state management
sim_state <- list(
  population = population_dt,         # Current population
  disease_history = disease_events,   # Disease onset records  
  exposure_history = exposure_trajectories,
  mortality_records = death_events,
  annual_summaries = summary_stats
)
```

#### Output Organization

Hierarchical output structure:

    outputs/
    ├── mc=1/                    # Monte Carlo iteration 1
    │   ├── population_final.fst
    │   ├── disease_events.fst
    │   └── mortality.fst
    ├── mc=2/                    # Monte Carlo iteration 2
    │   └── ...
    └── summaries/               # Aggregated across MC iterations
        ├── mortality_trends.csv
        ├── disease_incidence.csv
        └── exposure_patterns.csv

### Calibration Framework

#### Target Matching

Multi-objective calibration process:

``` r
# Calibration workflow
calibration_targets <- list(
  mortality_rates = japanese_mortality_data,
  disease_prevalence = nhanes_jp_data,
  exposure_distributions = dietary_survey_data
)

calibrate_model <- function(parameters) {
  # Run short simulation with candidate parameters
  results <- quick_simulation(parameters)
  
  # Calculate distance from targets
  distance <- calculate_distance(results, calibration_targets)
  
  return(distance)
}

# Optimization
optimal_params <- optim(
  par = initial_parameters,
  fn = calibrate_model,
  method = "L-BFGS-B"
)
```

#### Validation Metrics

Comprehensive validation against multiple data sources:

``` r
# Validation framework
validation_results <- validate_model(simulation_output) %>%
  check_mortality_trends() %>%
  check_disease_incidence() %>%
  check_exposure_correlations() %>%
  check_survival_curves() %>%
  generate_validation_report()
```

### Extensibility

#### Adding New Diseases

Framework designed for easy extension:

``` r
# Template for new disease
NewDisease <- R6Class("NewDisease", inherit = Disease,
  public = list(
    initialize = function() {
      super$initialize("new_disease_name")
      # Disease-specific initialization
    },
    
    calculate_incidence = function(population, year) {
      # Implement disease-specific incidence model
    },
    
    apply_case_fatality = function(cases, year) {
      # Implement disease-specific mortality
    }
  )
)
```

#### Adding New Risk Factors

Similar pattern for risk factors:

``` r
# Template for new exposure
NewExposure <- R6Class("NewExposure", inherit = Exposure,
  public = list(
    simulate_trajectory = function(population, years) {
      # Implement exposure-specific trajectory model
    },
    
    get_disease_effects = function() {
      # Return relative risks for associated diseases
    }
  )
)
```

### Performance Considerations

#### Memory Management

- **Efficient data structures**: `data.table` for large datasets
- **Memory monitoring**: Automatic garbage collection triggers
- **Streaming I/O**: Write results incrementally to avoid memory limits
- **Compression**: Use `fst` format for fast, compressed storage

#### Computational Optimization

- **Vectorized operations**: Leverage R’s vectorization
- **Sparse matrices**: For large transition probability matrices
- **Lazy evaluation**: Defer expensive calculations when possible
- **Caching**: Store frequently-accessed computed values

#### Scalability

- **Horizontal scaling**: Distribute across multiple machines
- **Vertical scaling**: Optimize for multi-core systems
- **Cloud deployment**: Support for containerized execution
- **Checkpointing**: Save/restore simulation state for long runs

### Quality Assurance

#### Testing Framework

Comprehensive test suite using `testthat`:

``` r
# Example test structure
test_that("Population synthesis produces valid demographics", {
  design <- create_test_design()
  synthpop <- SynthPop$new(design)
  
  expect_true(all(synthpop$population$age >= 20))
  expect_true(all(synthpop$population$age <= 100))
  expect_equal(sum(synthpop$population$sex == 0), 
               expected_male_count, tolerance = 100)
})
```

#### Continuous Integration

Automated testing on multiple platforms:

- **Unit tests**: Individual function validation
- **Integration tests**: Full simulation runs
- **Performance tests**: Runtime and memory benchmarks
- **Reproducibility tests**: Ensure consistent results across runs

#### Documentation Standards

- **Roxygen2**: Automated documentation generation
- **Vignettes**: Comprehensive user guides
- **Code coverage**: Monitor test coverage
- **API documentation**: Complete function reference

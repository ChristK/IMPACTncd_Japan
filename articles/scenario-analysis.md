# Scenario Analysis

## Scenario Analysis with IMPACTncd-Japan

This guide covers how to design, implement, and analyze policy scenarios
using IMPACTncd-Japan.

### What is Scenario Analysis?

Scenario analysis allows you to: - **Model policy interventions**: Test
the impact of health policies - **Compare alternatives**: Evaluate
different intervention strategies  
- **Quantify benefits**: Measure health gains and lives saved - **Assess
cost-effectiveness**: Support health economic evaluations

### Types of Scenarios

#### 1. Trend Modification Scenarios

Change the trajectory of risk factor trends over time:

``` yaml
# Example: Reduce smoking prevalence
scenario_name: "smoking_reduction"
interventions:
  smoking:
    type: "trend_change"
    start_year: 2025
    end_year: 2040
    target_reduction: 0.50    # 50% relative reduction
    implementation_curve: "linear"  # linear, exponential, or logistic
```

#### 2. Policy Shock Scenarios

Implement immediate changes to risk factors:

``` yaml
# Example: Sugar tax reducing BMI
scenario_name: "sugar_tax"
interventions:
  bmi:
    type: "policy_shock"
    implementation_year: 2030
    effect_size: -1.2         # Reduce mean BMI by 1.2 kg/m²
    coverage: 0.80            # 80% population coverage
    fadeout_years: 5          # Effect diminishes over 5 years
```

#### 3. Treatment/Care Improvements

Modify disease case fatality rates or treatment effectiveness:

``` yaml
# Example: Improved cardiac care
scenario_name: "improved_cardiac_care"
interventions:
  chd_treatment:
    type: "case_fatality_reduction"
    start_year: 2025
    diseases: ["CHD", "stroke"]
    reduction: 0.15           # 15% reduction in case fatality
    coverage_increase: 0.10   # 10% increase in treatment coverage
```

### Implementing Scenarios

#### Method 1: YAML Configuration

Create scenario-specific YAML files:

``` yaml
# scenarios/smoking_ban.yaml
scenario_name: "comprehensive_smoking_ban"
scenario_description: "Complete smoking ban in public places with enforcement"

base_design_file: "./inputs/sim_design_baseline.yaml"

interventions:
  smoking:
    type: "trend_change"
    start_year: 2025
    target_reduction: 0.70
    implementation_curve: "exponential"
    
  passive_smoking:
    type: "policy_shock" 
    implementation_year: 2025
    effect_size: -0.80        # 80% reduction in passive smoking
```

#### Method 2: Programmatic Definition

Define scenarios in R code:

``` r
create_smoking_scenario <- function(reduction_percent, start_year = 2025) {
  design <- Design$new()
  design$read_yaml("./inputs/sim_design_baseline.yaml")
  
  # Modify smoking trends
  design$scenario_prm$smoking_reduction <- reduction_percent
  design$scenario_prm$intervention_start <- start_year
  
  # Update scenario metadata
  design$sim_prm$scenario_name <- paste0("smoking_", reduction_percent*100, "pct")
  design$sim_prm$scenario_description <- paste0(
    "Smoking reduction of ", reduction_percent*100, "% starting ", start_year
  )
  
  return(design)
}

# Create multiple scenarios
scenarios <- list(
  smoking_25 = create_smoking_scenario(0.25),
  smoking_50 = create_smoking_scenario(0.50),
  smoking_75 = create_smoking_scenario(0.75)
)
```

#### Method 3: Batch Scenario Runner

Process multiple scenarios automatically:

``` r
library(yaml)

run_scenario_batch <- function(scenario_dir, output_base_dir) {
  scenario_files <- list.files(scenario_dir, pattern = "\\.yaml$", full.names = TRUE)
  
  results <- list()
  
  for (scenario_file in scenario_files) {
    scenario_name <- tools::file_path_sans_ext(basename(scenario_file))
    cat("Running scenario:", scenario_name, "\n")
    
    # Load scenario configuration
    scenario_config <- yaml::read_yaml(scenario_file)
    
    # Create design
    design <- Design$new()
    design$read_yaml(scenario_config$base_design_file)
    
    # Apply scenario modifications
    design$apply_scenario(scenario_config)
    
    # Set output directory
    design$sim_prm$output_dir <- file.path(output_base_dir, scenario_name)
    
    # Run simulation
    sim <- Simulation$new(design)
    sim$run()
    
    # Store results summary
    results[[scenario_name]] <- sim$get_summary()
  }
  
  return(results)
}
```

### Complex Multi-Intervention Scenarios

#### Combined Risk Factor Interventions

``` yaml
scenario_name: "comprehensive_prevention"
scenario_description: "Multi-risk factor intervention program"

interventions:
  # Tobacco control
  smoking:
    type: "trend_change"
    start_year: 2025
    target_reduction: 0.60
    
  # Nutrition policy  
  bmi:
    type: "policy_shock"
    implementation_year: 2027
    effect_size: -1.5
    
  # Physical activity promotion
  physical_activity:
    type: "trend_change"
    start_year: 2026
    target_increase: 0.30
    
  # Salt reduction
  blood_pressure:
    type: "policy_shock"
    implementation_year: 2028
    effect_size: -2.5         # mmHg reduction
```

#### Staged Implementation

``` yaml
scenario_name: "phased_intervention"

# Phase 1: Tobacco control (2025-2030)
phase_1:
  years: [2025, 2030]
  interventions:
    smoking:
      type: "trend_change"
      target_reduction: 0.40

# Phase 2: Add nutrition (2030-2035)  
phase_2:
  years: [2030, 2035]
  interventions:
    smoking:
      type: "maintain"        # Continue phase 1 effect
    bmi:
      type: "policy_shock"
      effect_size: -1.0

# Phase 3: Full implementation (2035+)
phase_3:
  years: [2035, 2040]
  interventions:
    smoking:
      type: "trend_change"
      target_reduction: 0.70  # Strengthen tobacco control
    bmi:
      type: "maintain"
    physical_activity:
      type: "trend_change"
      target_increase: 0.25
```

### Analyzing Scenario Results

#### Comparative Analysis

``` r
# Load results from multiple scenarios
baseline_results <- load_simulation_results("./outputs/baseline")
scenario_results <- load_simulation_results("./outputs/smoking_50pct")

# Calculate differences
health_impact <- calculate_health_impact(
  baseline = baseline_results,
  scenario = scenario_results,
  metrics = c("deaths_averted", "life_years_gained", "disease_cases_prevented")
)

# Summarize by age groups and time periods
summary_table <- health_impact %>%
  group_by(age_group, year) %>%
  summarise(
    deaths_averted = sum(deaths_averted),
    ly_gained = sum(life_years_gained),
    cases_prevented = sum(cases_prevented),
    .groups = "drop"
  )
```

#### Time Series Analysis

``` r
# Track intervention effects over time
plot_intervention_timeline <- function(results_list) {
  
  combined_data <- map_dfr(results_list, ~{
    .x$mortality_trends %>%
      select(year, deaths_total, scenario_name)
  }, .id = "scenario")
  
  ggplot(combined_data, aes(x = year, y = deaths_total, color = scenario)) +
    geom_line(size = 1.2) +
    geom_vline(xintercept = 2025, linetype = "dashed", alpha = 0.7) +
    labs(
      title = "Mortality Trends by Scenario",
      subtitle = "Intervention starts in 2025",
      x = "Year",
      y = "Total Deaths",
      color = "Scenario"
    ) +
    theme_minimal()
}
```

#### Cost-Effectiveness Preparation

``` r
# Prepare data for health economic evaluation
prepare_cea_data <- function(baseline, scenario, intervention_cost) {
  
  # Calculate health outcomes
  health_outcomes <- list(
    deaths_averted = sum(scenario$deaths_total) - sum(baseline$deaths_total),
    qalys_gained = sum(scenario$qalys_total) - sum(baseline$qalys_total),
    life_years_gained = sum(scenario$life_years_total) - sum(baseline$life_years_total)
  )
  
  # Calculate costs (requires additional cost modeling)
  costs <- list(
    intervention_cost = intervention_cost,
    healthcare_cost_savings = calculate_healthcare_savings(baseline, scenario),
    net_cost = intervention_cost - calculate_healthcare_savings(baseline, scenario)
  )
  
  # Cost-effectiveness ratios
  cea_ratios <- list(
    cost_per_qaly = costs$net_cost / health_outcomes$qalys_gained,
    cost_per_ly = costs$net_cost / health_outcomes$life_years_gained,
    cost_per_death_averted = costs$net_cost / health_outcomes$deaths_averted
  )
  
  return(list(
    outcomes = health_outcomes,
    costs = costs,
    ratios = cea_ratios
  ))
}
```

### Best Practices

#### Scenario Design

1.  **Use realistic assumptions**: Base interventions on evidence from
    similar policies
2.  **Consider implementation lag**: Allow time for policy rollout and
    behavior change
3.  **Account for coverage**: Not all interventions reach 100% of the
    population
4.  **Model fadeout effects**: Consider whether intervention effects
    diminish over time

#### Validation

``` r
# Validate scenario assumptions
validate_scenario <- function(design) {
  checks <- list()
  
  # Check intervention magnitudes are realistic
  checks$realistic_effects <- all(abs(design$intervention_effects) < 0.8)
  
  # Check timeline consistency
  checks$timeline_valid <- design$intervention_start >= design$sim_prm$years[1]
  
  # Check coverage rates
  checks$coverage_valid <- all(design$coverage_rates <= 1.0 & design$coverage_rates >= 0)
  
  return(checks)
}
```

#### Sensitivity Analysis

``` r
# Test robustness of results
run_sensitivity_analysis <- function(base_scenario, parameters, ranges) {
  
  results <- list()
  
  for (param in parameters) {
    param_results <- list()
    
    for (value in ranges[[param]]) {
      # Modify parameter
      modified_scenario <- base_scenario
      modified_scenario[[param]] <- value
      
      # Run simulation
      sim <- Simulation$new(modified_scenario)
      sim$run()
      
      param_results[[as.character(value)]] <- sim$get_summary()
    }
    
    results[[param]] <- param_results
  }
  
  return(results)
}

# Example sensitivity analysis
sensitivity_params <- list(
  smoking_reduction = seq(0.3, 0.7, 0.1),
  implementation_speed = c("slow", "medium", "fast"),
  coverage_rate = seq(0.6, 1.0, 0.1)
)
```

### Reporting Results

#### Summary Tables

Generate standardized summary tables for different audiences:

``` r
# Policy maker summary
create_policy_summary <- function(scenario_results) {
  summary <- scenario_results %>%
    group_by(scenario_name) %>%
    summarise(
      `Deaths Averted (2025-2040)` = sum(deaths_averted),
      `Life Years Gained` = sum(life_years_gained),
      `Disease Cases Prevented` = sum(cases_prevented),
      `Peak Health Benefit Year` = year[which.max(annual_benefit)],
      .groups = "drop"
    )
  
  return(summary)
}

# Technical summary
create_technical_summary <- function(scenario_results) {
  # Include confidence intervals, age-specific results, etc.
}
```

#### Visualization

Create compelling visualizations for different stakeholders:

``` r
# Create impact visualization
plot_scenario_impact <- function(results) {
  # Implementation depends on your specific data structure
  # Include before/after comparisons, uncertainty bands, etc.
}
```

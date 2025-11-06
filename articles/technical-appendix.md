# Technical Documentation

## Introduction

IMPACT NCD is an open-source microsimulation modelling framework for
public health policy evaluation, planning, and decision-making focused
on the prevention of non-communicable diseases (NCD). The present paper
aimed to evaluate the effect of observed changes in cardiovascular
disease (CVD) risk factors on CVD mortality, prevalence, incidence,
medical costs, and Quality Adjusted Life Years (QALYs) between 2001 and
2019.

IMPACT NCD Japan (IMPACT NCD-JPN) is presented for the first time, while
IMPACT NCD has been used extensively to model primary prevention
policies nationally in England, Brazil, Germany, and the United States
(US) and locally in Liverpool, a city in the northwest of England. At
the core of the IMPACT NCD-JPN model is an epidemiological engine that
includes age, sex, smoking, fruit and vegetable consumption, physical
activity, body mass index (BMI), systolic blood pressure (SBP),
haemoglobin A1c (HbA1c), and low-density lipoprotein cholesterol
(LDL-c), as risk factors.

> **Note**: The model architecture description of the IMPACT NCD-JPN in
> this document is based on an earlier technical appendix of IMPACT NCD
> England published by the University of Liverpool. Thus, some of the
> text is similar to that in the earlier document.

## Epidemiological Engine

### High-level Description

The epidemiological engine of IMPACT NCD-JPN is a discrete-time,
dynamic, stochastic microsimulation consisting of three modules: **the
Demographic, Exposure, and Disease modules**.

Within the IMPACT NCD-JPN epidemiological engine, each unit is a
synthetic individual (simulant) represented by a record containing a
unique identifier and a set of associated attributes. The
microsimulation then projects the life course of each synthetic
individual.

Each synthetic individual’s attributes include:

1.  **Age and sex** as demographic exposures.

2.  **Behavioral risk factors**: Smoking status (current
    smoker/ex-smoker/never-smoker), smoking intensity, fruit and
    vegetable consumption, and physical activity.

3.  **Biological risk factors**: BMI, SBP, LDL-c, and HbA1c.

4.  **Disease risk**: The risk for two diseases is modelled explicitly:
    CHD and stroke. The risk for these conditions is modelled from
    relevant exposures to demographic, behavioural, and biological risk
    factor attributes.

5.  **Mortality**: Mortality from CHD, stroke, or any other cause is
    recorded if it occurs.

All these attributes are updated in discrete annual steps according to a
set of stochastic rules based on well-established epidemiological
principles.

### Demographic Module

The first year of every simulation in IMPACT NCD-JPN is 2001. For each
simulation, the algorithm:

1.  Draws 400,000 synthetic individuals, aged 30 to 99, from the joint
    age-sex distribution (this is a default value that can be modified
    by the user).

2.  Creates backward projections to 1991 (maximum time lag of ten
    years).

3.  Creates forward projections until 2019, with mortality
    considerations.

IMPACT NCD-JPN follows an open cohort approach where new 30-year-old
cohorts enter the model annually.

### Exposure Module

This module simulates adult life course exposures based on the National
Health and Nutrition Survey (NHNS) series between 2001 and 2019. The
approach follows these principles:

1.  **Statistical modeling**: Fit appropriate models with exposure as
    dependent variable and functions of year, age, sex, and other
    exposures as independent variables.

2.  **Trend extraction**: Include year as independent variable to
    extract trends and project into the future.

3.  **Rank stability**: Synthetic individuals retain their quantiles
    throughout their life course, with minor random walk adjustments.

#### Clustering of Risk Factors

The model captures correlations between exposures using:

1.  Imputation of missing variables in NHNS
2.  Conversion of exposures to quantiles
3.  Estimation of linear correlation matrix
4.  Generation of correlated random number streams
5.  Application of correlated streams as exposure quantiles

### Disease Module

The disease module translates exposures to disease incidence using a
population-attributable risk fraction approach (PARF).

#### Disease Incidence

The model follows a 3-step approach:

**Step 1**: Estimate incidence proportion attributable to each modelled
risk factor by age and sex, assuming specific time lags between exposure
and disease.

**Step 2**: Estimate the portion of disease incidence attributable to
all modelled risk factors, assuming multiplicative risks.

**Step 3**: Estimate individualized probability of developing disease
for each synthetic individual.

The implementation uses the PARF formula:

    PARF = 1 - n/Σ(RRi1 * RRi2 * ... * RRik)

Where: - n = number of synthetic individuals in the population - RRi1…ik
= relative risks of risk factors for each individual i

#### Time Lags

Time lags vary stochastically between 1 and 10 years following a shifted
binomial distribution, with mean lag times set according to empirical
data:

| Risk Factor       | CHD | Stroke |
|-------------------|-----|--------|
| Physical activity | 4   | 4      |
| BMI               | 4   | 3      |
| Fruit/vegetable   | 4   | 4      |
| SBP               | 4   | 4      |
| Smoking           | 4   | 4      |
| LDL-c             | 4   | 4      |
| HbA1c             | 4   | 4      |

#### Initial Prevalence and Mortality

For the initial simulation year, synthetic individuals are allocated as
prevalent cases using similar procedures as for incidence. The model
treats mortality from any cause using disease-specific mortality rates
and applies the same PARF approach to case fatality.

## Model Outputs

The model produces adult life-course trajectories for each simulant over
a 19-year time horizon (2001-2019). Annual summary measures including
incidence, prevalence, and mortality rates are calculated and can be
compared across scenarios.

### Uncertainty Analysis

IMPACT NCD-JPN implements a 2nd order Monte Carlo approach to estimate
uncertainty intervals, considering:

- **Stochastic uncertainty**: Minimized by using same random numbers
  across scenarios
- **Parameter uncertainty**: From sampling errors in relative risks
- **Individual heterogeneity**: Conditional on individual
  characteristics
- **Structural uncertainty**: Limited consideration due to strong
  epidemiological foundations

## Validation and Calibration

The model is validated using:

1.  **Internal validation**: Plotting modelled exposures against
    observed NHNS data
2.  **Disease validation**: Comparing modelled disease incidence to
    observed data
3.  **Calibration**: Adjusting modelled incidence to observed data for
    2001-2019

Trends in disease incidence are decomposed based on causally linked risk
factors, with residual incidence assumed to follow log-linear trends.

## Data Sources

### Exposure Data

- **National Health and Nutrition Survey (NHNS)**: Risk factor levels,
  2001-2019
- **Population**: Statistics Bureau, Ministry of Internal Affairs and
  Communications

### Disease Data

- **Incidence/Prevalence**: Global Burden of Diseases (GBD) 2021 results
- **Mortality**: Vital Statistics Japan, Ministry of Health, Labour and
  Welfare
- **Projections**: Bayesian age-period-cohort models

### Cost Data

- **Direct costs**: National Medical Care Expenditure estimates (2019)
- **Indirect costs**: Cost of illness study for CHD and stroke (2017)

## Key Assumptions and Limitations

### Model Assumptions

| Component       | Key Assumptions                                                                                     |
|-----------------|-----------------------------------------------------------------------------------------------------|
| **Demographic** | Migration not explicitly modelled; calibrated to official projections                               |
| **Exposure**    | Surveys representative; rank stability; constant correlations over time                             |
| **Disease**     | Multiplicative risk effects; log-linear exposure-response; 100% risk reversibility (except smoking) |

### Limitations

1.  **Data limitations**: Pre-COVID-19 data only; limited to 2019
2.  **Risk factors**: Limited to modelled factors; incomplete smoking
    cumulative risk
3.  **Selection bias**: Unavoidable in data sources
4.  **Discrete-time bias**: Annual time steps may miss within-year
    dynamics

## Strengths

The IMPACT NCD-JPN model provides:

1.  **Dynamic modeling**: Accounts for competing risks and population
    dynamics
2.  **Individual-level analysis**: Enables exploration of multimorbidity
    patterns
3.  **Flexibility**: Modular design allows independent development of
    policy layers
4.  **Transparency**: Open-source license ensures accountability
5.  **Validation**: Extensive internal and external validation
    procedures

## Mathematical Framework Details

### Statistical Models Used

| Exposure         | Statistical Model           | Distribution                   |
|------------------|-----------------------------|--------------------------------|
| Active days      | Ordinal logistic regression | \-                             |
| Fruit/vegetables | GAMLSS                      | Zero Altered Negative Binomial |
| Smoking status   | GAMLSS                      | Binomial                       |
| BMI              | GAMLSS                      | Box-Cox t with log link        |
| HbA1c            | GAMLSS                      | Box-Cox t                      |
| SBP              | GAMLSS                      | Box-Cox Power Exponential      |
| LDL-c            | GAMLSS                      | Box-Cox t                      |

### Reference Levels for Risk Calculation

- **SBP**: Below 112.5 mmHg (110-115 range)
- **LDL-c**: Below 85 mg/dL (70-100 range)  
- **BMI**: Below 22 kg/m² (21-23 range)
- **HbA1c**: Below 6.5% (6-7 range)
- **Fruit/vegetables**: 4+ portions per day (3-5 range)
- **Physical activity**: 5+ active days per week for CHD

## Cost Analysis

### Direct Costs

Sourced from National Medical Care Expenditure estimates for 2019,
covering medical treatment costs for CHD and stroke.

### Indirect Costs

- **Productivity losses**: Morbidity and mortality-related losses for
  ages 30-75
- **Informal care**: Family burden of long-term care across age groups
- **Apportionment**: Based on working population proportions and disease
  prevalence

## Disease-Specific Details

### Coronary Heart Disease (CHD)

**Model Structure**: Chronic condition with no recovery

**Risk Factors**: Smoking, fruit/vegetable intake, physical activity,
BMI, SBP, LDL-c, HbA1c

**Validation**: Extensive validation plots comparing modeled
vs. observed incidence and mortality by age and sex

### Stroke

**Model Structure**: Includes ischemic and hemorrhagic stroke (including
subarachnoid hemorrhage)

**Risk Factors**: Same as CHD

**Validation**: Comparison with local registries (Kochi Prefecture
stroke registry, Kumamoto AMI registry)

## References

The model incorporates evidence from multiple systematic reviews and
meta-analyses for relative risk estimates, with specific sources
detailed in the supplementary materials. Key data sources include GBD
2021, Japanese national surveys, and international epidemiological
studies.

## 1. Model Overview

### 1.1 Model Structure

The IMPACTncd-Japan model is a closed-cohort microsimulation model that
simulates the lifecourse of a synthetic population of Japanese adults.
The model tracks individuals from age 20 to death, modeling:

- **Risk factor trajectories**: Smoking, BMI, blood pressure,
  cholesterol, physical activity
- **Disease incidence**: CHD, stroke, diabetes, COPD, lung cancer
- **Disease progression**: Including complications and case fatality
- **Mortality**: Disease-specific and all-cause mortality

### 1.2 Conceptual Framework

*\[This section would include details from the PDF about the conceptual
framework, model flow diagrams, etc.\]*

### 1.3 Model Population

*\[Details about the synthetic population generation, demographics,
etc.\]*

## 2. Risk Factor Modeling

### 2.1 Smoking

#### 2.1.1 Smoking Initiation

*\[Mathematical formulations for smoking initiation rates\]*

``` r
# Example structure for smoking initiation model
# logit(P_initiation) = β₀ + β₁×age + β₂×sex + β₃×education + ...
```

#### 2.1.2 Smoking Cessation

*\[Mathematical formulations for smoking cessation rates\]*

#### 2.1.3 Smoking Intensity

*\[Modeling of cigarettes per day among smokers\]*

### 2.2 Body Mass Index (BMI)

#### 2.2.1 BMI Trajectories

*\[Mathematical formulations for BMI change over time\]*

#### 2.2.2 Obesity Transitions

*\[Modeling transitions between BMI categories\]*

### 2.3 Blood Pressure

#### 2.3.1 Systolic Blood Pressure Model

*\[Continuous modeling of systolic blood pressure\]*

#### 2.3.2 Hypertension Incidence

*\[Binary modeling of hypertension diagnosis\]*

### 2.4 Cholesterol

#### 2.4.1 Total Cholesterol Model

*\[Continuous modeling of total cholesterol\]*

#### 2.4.2 Treatment Effects

*\[Modeling of statin and other lipid-lowering treatments\]*

### 2.5 Physical Activity

#### 2.5.1 Physical Activity Levels

*\[Modeling of physical activity patterns\]*

#### 2.5.2 Sedentary Behavior

*\[Modeling of sedentary time\]*

## 3. Disease Models

### 3.1 Coronary Heart Disease (CHD)

#### 3.1.1 Incidence Model

*\[Mathematical formulation for CHD incidence\]*

``` r
# Example CHD incidence model structure
# log(hazard_CHD) = β₀ + β₁×age + β₂×sex + β₃×smoking + β₄×SBP + β₅×cholesterol + ...
```

#### 3.1.2 Case Fatality

*\[CHD case fatality rates and time trends\]*

#### 3.1.3 Recurrent Events

*\[Modeling of recurrent CHD events\]*

### 3.2 Stroke

#### 3.2.1 Stroke Incidence

*\[Mathematical formulation for stroke incidence\]*

#### 3.2.2 Stroke Subtypes

*\[Ischemic vs hemorrhagic stroke modeling\]*

#### 3.2.3 Stroke Survival

*\[Post-stroke survival modeling\]*

### 3.3 Diabetes

#### 3.3.1 Type 2 Diabetes Incidence

*\[Mathematical formulation for diabetes incidence\]*

#### 3.3.2 Diabetes Complications

*\[Modeling of diabetic complications\]*

#### 3.3.3 Diabetes Management

*\[Treatment effects and control modeling\]*

### 3.4 Chronic Obstructive Pulmonary Disease (COPD)

#### 3.4.1 COPD Incidence

*\[Mathematical formulation for COPD incidence\]*

#### 3.4.2 COPD Progression

*\[Disease severity progression modeling\]*

### 3.5 Lung Cancer

#### 3.5.1 Lung Cancer Incidence

*\[Mathematical formulation for lung cancer incidence\]*

#### 3.5.2 Lung Cancer Survival

*\[Survival modeling by stage and treatment\]*

## 4. Mortality Modeling

### 4.1 Background Mortality

#### 4.1.1 All-Cause Mortality

*\[Background mortality rates by age and sex\]*

#### 4.1.2 Competing Risks

*\[Handling of competing risks in the simulation\]*

### 4.2 Disease-Specific Mortality

#### 4.2.1 Relative Risk Models

*\[Disease-specific mortality relative risks\]*

#### 4.2.2 Temporal Trends

*\[Time trends in case fatality rates\]*

## 5. Data Sources and Calibration

### 5.1 Input Data Sources

#### 5.1.1 Population Demographics

*\[Details of population data sources\]*

#### 5.1.2 Risk Factor Distributions

*\[Sources for risk factor prevalence and trends\]*

#### 5.1.3 Disease Incidence Data

*\[Sources for disease incidence rates\]*

#### 5.1.4 Mortality Data

*\[Sources for mortality statistics\]*

### 5.2 Calibration Methods

#### 5.2.1 Calibration Targets

*\[Description of calibration targets and methods\]*

#### 5.2.2 Calibration Algorithms

*\[Mathematical description of calibration procedures\]*

#### 5.2.3 Validation Metrics

*\[Metrics used to assess model performance\]*

## 6. Model Implementation

### 6.1 Software Architecture

#### 6.1.1 R Package Structure

*\[Description of R package organization\]*

#### 6.1.2 C++ Implementation

*\[Details of C++ components for performance\]*

#### 6.1.3 Parallel Processing

*\[Implementation of parallel simulation\]*

### 6.2 Simulation Algorithm

#### 6.2.1 Main Simulation Loop

*\[Pseudocode for main simulation algorithm\]*

``` r
# Pseudocode for main simulation loop
for (year in simulation_years) {
  for (individual in population) {
    # Update age
    individual$age <- individual$age + 1
    
    # Update risk factors
    individual <- update_risk_factors(individual)
    
    # Check for disease incidence
    individual <- check_disease_incidence(individual)
    
    # Update disease progression
    individual <- update_disease_states(individual)
    
    # Check for mortality
    individual <- check_mortality(individual)
  }
  
  # Export annual results
  export_annual_results(year)
}
```

#### 6.2.2 Random Number Generation

*\[Details of random number generation and reproducibility\]*

#### 6.2.3 Memory Management

*\[Strategies for efficient memory usage\]*

### 6.3 Output Generation

#### 6.3.1 Summary Statistics

*\[Methods for generating summary outputs\]*

#### 6.3.2 Confidence Intervals

*\[Calculation of uncertainty bounds\]*

#### 6.3.3 Export Formats

*\[Description of output file formats\]*

## 7. Model Parameters

### 7.1 Risk Factor Parameters

#### 7.1.1 Smoking Parameters

*\[Complete list of smoking-related parameters\]*

#### 7.1.2 BMI Parameters

*\[Complete list of BMI-related parameters\]*

#### 7.1.3 Blood Pressure Parameters

*\[Complete list of blood pressure parameters\]*

### 7.2 Disease Parameters

#### 7.2.1 CHD Parameters

*\[Complete list of CHD model parameters\]*

#### 7.2.2 Stroke Parameters

*\[Complete list of stroke model parameters\]*

#### 7.2.3 Diabetes Parameters

*\[Complete list of diabetes model parameters\]*

### 7.3 Mortality Parameters

#### 7.3.1 Background Mortality Tables

*\[Life tables and mortality parameters\]*

#### 7.3.2 Relative Risk Parameters

*\[Disease-specific relative risk parameters\]*

## 8. Validation and Sensitivity Analysis

### 8.1 Internal Validation

#### 8.1.1 Face Validity

*\[Assessment of model behavior and outputs\]*

#### 8.1.2 Verification Tests

*\[Technical verification of implementation\]*

### 8.2 External Validation

#### 8.2.1 Historical Validation

*\[Comparison with historical trends\]*

#### 8.2.2 Cross-Validation

*\[Validation against external data sources\]*

### 8.3 Sensitivity Analysis

#### 8.3.1 Parameter Sensitivity

*\[Sensitivity to key parameter values\]*

#### 8.3.2 Structural Sensitivity

*\[Sensitivity to model structure assumptions\]*

## 9. Limitations and Assumptions

### 9.1 Model Limitations

#### 9.1.1 Scope Limitations

*\[Diseases and risk factors not included\]*

#### 9.1.2 Data Limitations

*\[Limitations of input data sources\]*

#### 9.1.3 Methodological Limitations

*\[Limitations of modeling approach\]*

### 9.2 Key Assumptions

#### 9.2.1 Risk Factor Assumptions

*\[Key assumptions about risk factor modeling\]*

#### 9.2.2 Disease Model Assumptions

*\[Key assumptions about disease modeling\]*

#### 9.2.3 Intervention Assumptions

*\[Assumptions about policy interventions\]*

## 10. Future Developments

### 10.1 Model Extensions

#### 10.1.1 Additional Diseases

*\[Plans for including additional diseases\]*

#### 10.1.2 Additional Risk Factors

*\[Plans for additional risk factor modeling\]*

### 10.2 Methodological Improvements

#### 10.2.1 Statistical Methods

*\[Potential improvements to statistical methods\]*

#### 10.2.2 Computational Efficiency

*\[Plans for improving computational performance\]*

## References

*\[Complete reference list for technical documentation\]*

------------------------------------------------------------------------

## Appendices

### Appendix A: Mathematical Notation

*\[Definition of mathematical symbols and notation\]*

### Appendix B: Parameter Tables

*\[Complete parameter tables\]*

### Appendix C: Validation Results

*\[Detailed validation results and figures\]*

### Appendix D: Software Dependencies

*\[Complete list of software dependencies and versions\]*

------------------------------------------------------------------------

*This technical appendix is based on the supplementary material
published with Kypridemos et al. (2025). For the most current version,
please refer to the original publication.*

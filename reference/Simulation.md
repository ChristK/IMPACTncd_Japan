# R6 Class representing a simulation environment

A simulation environment.

## Details

To be completed...

## Note

The calibration parameters are stored in
\`./simulation/calibration_prms.csv\` and are automatically applied in
subsequent simulations.

## Calibration Process

The calibration operates over a specific time period defined by two key
parameters:

- \*\*Start year\*\*: The initial year for in the designated sim_design
  yaml file

- \*\*End year\*\*: The latest year as defined by the sim_horizon in
  sim_design yaml file

The calibration process follows these steps for each age from \`ageL\`
to \`ageH\`:

### 1. Incidence Calibration

For both CHD and stroke:

- Runs simulation and extracts uncalibrated incidence rates

- Fits log-log linear models to both benchmark and simulated data over
  the calibration period

- Calculates calibration factors to align simulated trends with
  benchmark trends

- Updates prevalence estimates based on incidence corrections

### 2. Case Fatality Calibration

For CHD, stroke, and non-modeled causes:

- Incorporates incidence-corrected prevalence estimates

- Calculates calibration factors for case fatality rates

- Ensures mortality rates align with benchmark data

- Applies competing risk adjustments for previous ages

## Implementation Details

- \*\*Sequential Processing\*\*: Ages are processed sequentially to
  ensure proper prevalence carryover between age groups

- \*\*Trend Modeling\*\*: Uses log-log linear regression to capture
  temporal trends in the calibration period

- \*\*Robust Estimation\*\*: Employs median-based estimators to handle
  rare events and reduce Monte Carlo variability

## Public fields

- `design`:

  A Design object.

- `diseases`:

  A list of Disease objects.

- `RR`:

  A list of RR for the simulated exposures.

- `scenarios`:

  A list of scenario objects.

## Methods

### Public methods

- [`Simulation$new()`](#method-Simulation-new)

- [`Simulation$update_primary_prevention_scn()`](#method-Simulation-update_primary_prevention_scn)

- [`Simulation$get_primary_prevention_scn()`](#method-Simulation-get_primary_prevention_scn)

- [`Simulation$update_secondary_prevention_scn()`](#method-Simulation-update_secondary_prevention_scn)

- [`Simulation$get_secondary_prevention_scn()`](#method-Simulation-get_secondary_prevention_scn)

- [`Simulation$run()`](#method-Simulation-run)

- [`Simulation$calibrate_incd_ftlt()`](#method-Simulation-calibrate_incd_ftlt)

- [`Simulation$export_summaries()`](#method-Simulation-export_summaries)

- [`Simulation$export_tables()`](#method-Simulation-export_tables)

- [`Simulation$get_causal_structure()`](#method-Simulation-get_causal_structure)

- [`Simulation$get_node_names()`](#method-Simulation-get_node_names)

- [`Simulation$get_causal_path()`](#method-Simulation-get_causal_path)

- [`Simulation$update_design()`](#method-Simulation-update_design)

- [`Simulation$del_outputs()`](#method-Simulation-del_outputs)

- [`Simulation$del_summaries()`](#method-Simulation-del_summaries)

- [`Simulation$del_logs()`](#method-Simulation-del_logs)

- [`Simulation$del_parfs()`](#method-Simulation-del_parfs)

- [`Simulation$del_RR_cache()`](#method-Simulation-del_RR_cache)

- [`Simulation$del_synthpops()`](#method-Simulation-del_synthpops)

- [`Simulation$get_esp()`](#method-Simulation-get_esp)

- [`Simulation$get_mm_weights()`](#method-Simulation-get_mm_weights)

- [`Simulation$validate()`](#method-Simulation-validate)

- [`Simulation$split_large_files()`](#method-Simulation-split_large_files)

- [`Simulation$reconstruct_large_files()`](#method-Simulation-reconstruct_large_files)

- [`Simulation$del_large_files()`](#method-Simulation-del_large_files)

- [`Simulation$print()`](#method-Simulation-print)

- [`Simulation$clone()`](#method-Simulation-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new simulation object.

#### Usage

    Simulation$new(sim_prm)

#### Arguments

- `sim_prm`:

  Either a path to a yaml file or a Design object.

#### Returns

A new \`Simulation\` object.

------------------------------------------------------------------------

### Method `update_primary_prevention_scn()`

Updates the primary prevention policy scenario

#### Usage

    Simulation$update_primary_prevention_scn(method)

#### Arguments

- `method`:

  a function with synthpop as an argument that models the primary
  prevention policy.

#### Returns

The invisible self for chaining.

------------------------------------------------------------------------

### Method `get_primary_prevention_scn()`

Get the primary prevention policy scenario

#### Usage

    Simulation$get_primary_prevention_scn()

#### Returns

The primary prevention policy scenario.

------------------------------------------------------------------------

### Method `update_secondary_prevention_scn()`

Updates the secondary prevention policy scenario

#### Usage

    Simulation$update_secondary_prevention_scn(method)

#### Arguments

- `method`:

  a function with synthpop as an argument that models the secondary
  prevention policy.

#### Returns

The invisible self for chaining.

------------------------------------------------------------------------

### Method `get_secondary_prevention_scn()`

Get the secondary prevention policy scenario

#### Usage

    Simulation$get_secondary_prevention_scn()

#### Returns

The secondary prevention policy scenario.

------------------------------------------------------------------------

### Method `run()`

Runs a simulation

#### Usage

    Simulation$run(mc, multicore = TRUE, scenario_nam)

#### Arguments

- `mc`:

  A positive sequential integer vector with the Monte Carlo iterations
  of synthetic population to simulate, or a scalar.

- `multicore`:

  If TRUE run the simulation in parallel.

- `scenario_nam`:

  A string for the scenario name (i.e. sc1)

#### Returns

The invisible self for chaining.

------------------------------------------------------------------------

### Method `calibrate_incd_ftlt()`

Generates new calibration parameters for incidence and case fatality
rates.

This method implements a sequential age-based calibration process that
aligns modeled disease incidence and case fatality rates with user input
data from external sources (e.g., GBD, national statistics). The
calibration ensures that simulation outputs match observed
epidemiological patterns.

#### Usage

    Simulation$calibrate_incd_ftlt(mc, replace = FALSE)

#### Arguments

- `mc`:

  A positive sequential integer vector with the Monte Carlo iterations
  of synthetic population to simulate, or a scalar.

- `replace`:

  If TRUE the calibration deletes the previous calibration file and
  starts from scratch. If FALSE, continues from the last uncalibrated
  age.

#### Returns

The invisible self for chaining.

------------------------------------------------------------------------

### Method `export_summaries()`

Process the lifecourse files

#### Usage

    Simulation$export_summaries(
      multicore = TRUE,
      type = c("le", "hle", "dis_char", "prvl", "incd", "dis_mrtl", "mrtl",
        "all_cause_mrtl_by_dis", "cms", "qalys", "costs"),
      single_year_of_age = FALSE
    )

#### Arguments

- `multicore`:

  If TRUE run the simulation in parallel.

- `type`:

  The type of summary to extract.

- `single_year_of_age`:

  Export summaries by single year of age. Useful for the calibration
  proccess.

#### Returns

The invisible self for chaining.

------------------------------------------------------------------------

### Method `export_tables()`

Export summary tables for the simulation results.

This method generates and exports summary tables for the main simulation
outputs, including prevalence, incidence, mortality, disease
characteristics, and exposures. It calls modular helper methods for each
type of summary, ensuring output directories are created as needed and
that all tables are written to the appropriate locations.

#### Usage

    Simulation$export_tables(
      baseline_year_for_change_outputs = 2019L,
      prbl = c(0.5, 0.025, 0.975, 0.1, 0.9)
    )

#### Arguments

- `baseline_year_for_change_outputs`:

  Integer. The baseline year to use for change outputs (default: 2019L).

- `prbl`:

  Numeric vector. The quantiles to use for summary statistics (default:
  c(0.5, 0.025, 0.975, 0.1, 0.9)).

#### Details

This method is a high-level wrapper that orchestrates the export of all
main summary tables. It delegates the actual export logic to the
following private helper methods: - `private$export_main_tables` -
`private$export_all_cause_mrtl_tables` -
`private$export_disease_characteristics_tables` -
`private$export_xps_tables`

Each helper method is responsible for a specific set of outputs and
ensures that the results are saved in the correct format and location.

#### Returns

The invisible self for chaining.

#### Examples

    IMPACTncd$export_tables()

------------------------------------------------------------------------

### Method `get_causal_structure()`

Returns the causality matrix and optionally plots the causality
structure.

#### Usage

    Simulation$get_causal_structure(
      processed = TRUE,
      print_plot = FALSE,
      focus = FALSE
    )

#### Arguments

- `processed`:

  If \`TRUE\` generates the causality matrix from the graph.

- `print_plot`:

  If \`TRUE\` prints the causal structure graph.

- `focus`:

  If missing the whole causal structure is returned. Otherwise, if a
  named node only the subgraph of the 1st order neighbours that point to
  the given vertrice is returned.

#### Returns

The processed causality matrix if \`processed = TRUE\` or the graph
otherwise.

------------------------------------------------------------------------

### Method `get_node_names()`

Returns the names of all exposures and diseases.

#### Usage

    Simulation$get_node_names()

#### Returns

A string vector.

------------------------------------------------------------------------

### Method `get_causal_path()`

Returns the causal paths between an exposure and an outcome (disease).

#### Usage

    Simulation$get_causal_path(from, to, shortest_paths = FALSE)

#### Arguments

- `from`:

  the beginning of the path (an exposure) as a string. Use
  \`get_node_names\` for available nodes.

- `to`:

  the end of the path (a disease) as a string. Use \`get_node_names\`
  for available nodes.

- `shortest_paths`:

  Boolean. If true, only returns the paths with the smallest number of
  nodes. Else, all possible paths (excluding multiple and loop edges)
  are returned.

#### Returns

A list with all the possible paths between exposure and disease.

------------------------------------------------------------------------

### Method `update_design()`

Updates the Design object that is stored in the Simulation object.

#### Usage

    Simulation$update_design(new_design)

#### Arguments

- `new_design`:

  A design object with the simulation parameters.

#### Returns

The invisible self for chaining.

------------------------------------------------------------------------

### Method `del_outputs()`

Delete all output files and folders below the first level.

#### Usage

    Simulation$del_outputs()

#### Returns

The invisible self for chaining.

------------------------------------------------------------------------

### Method `del_summaries()`

Delete all output summary files and subdirectories while preserving
first-level directory structure.

#### Usage

    Simulation$del_summaries()

#### Returns

The invisible self for chaining.

------------------------------------------------------------------------

### Method `del_logs()`

Delete log files.

#### Usage

    Simulation$del_logs()

#### Returns

The invisible self for chaining.

------------------------------------------------------------------------

### Method `del_parfs()`

Delete all files in the ./simulation/parf folder.

#### Usage

    Simulation$del_parfs()

#### Returns

The invisible self for chaining.

------------------------------------------------------------------------

### Method `del_RR_cache()`

Delete all files in the ./simulation/rr folder.

#### Usage

    Simulation$del_RR_cache()

#### Returns

The invisible self for chaining.

------------------------------------------------------------------------

### Method `del_synthpops()`

Delete all files in the synthpop folder.

#### Usage

    Simulation$del_synthpops()

#### Returns

The invisible self for chaining.

------------------------------------------------------------------------

### Method `get_esp()`

Get the European Standardised Population 2013 by sex and dimd.

#### Usage

    Simulation$get_esp()

#### Returns

A data.table with the European Standardised Population 2013.

------------------------------------------------------------------------

### Method `get_mm_weights()`

Get the disease multimorbidity weights (i.e. Cambridge Morbidity Score
weights).

#### Usage

    Simulation$get_mm_weights()

#### Returns

A named vector with disease weights.

------------------------------------------------------------------------

### Method `validate()`

Internal validation of the disease burden.

#### Usage

    Simulation$validate()

#### Returns

The invisible self for chaining.

------------------------------------------------------------------------

### Method `split_large_files()`

Splits files larger than 50Mb into chunks of 49Mb.

#### Usage

    Simulation$split_large_files()

#### Details

The function splits files larger than 50Mb into chunks of 49Mb so they
can be tracked by GitHub. The large files are deleted and an index is
created at "./simulation/large_files_indx.csv" so they can be
reconstructed. The function also adds the large files to \`.gitignore\`.
It works on Linux and Windows. Untested on Mac.

#### Returns

The invisible \`Simulation\` object.

------------------------------------------------------------------------

### Method `reconstruct_large_files()`

Reconstructs large files from chunks.

#### Usage

    Simulation$reconstruct_large_files()

#### Details

The function reconstructs large files from chunks. The path of the files
are stored in "./simulation/large_files_indx.csv". It works on Linux and
Windows. Untested on Mac.

#### Returns

The invisible \`Simulation\` object.

------------------------------------------------------------------------

### Method `del_large_files()`

Deletes large files.

#### Usage

    Simulation$del_large_files()

#### Details

The function deletes large files that are stored in chunks. The path of
the files are stored in "./simulation/large_files_indx.csv".

#### Returns

The invisible \`Simulation\` object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Prints the simulation object metadata.

#### Usage

    Simulation$print()

#### Returns

The invisible \`Simulation\` object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Simulation$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
## ------------------------------------------------
## Method `Simulation$export_tables`
## ------------------------------------------------

IMPACTncd$export_tables()
#> Error: object 'IMPACTncd' not found
```

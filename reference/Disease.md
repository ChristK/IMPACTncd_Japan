# R6 Class representing an disease

A disease has a sim_prm list that holds the simulation parameters.

## Details

To be completed...

## Public fields

- `name`:

  The name of the disease.

- `friendly_name`:

  A friendly name for the disease.

- `meta`:

  Disease metadata including type.

- `notes`:

  Any notes regarding the disease.

## Methods

### Public methods

- [`Disease$new()`](#method-Disease-new)

- [`Disease$gen_parf_files()`](#method-Disease-gen_parf_files)

- [`Disease$gen_parf()`](#method-Disease-gen_parf)

- [`Disease$set_init_prvl()`](#method-Disease-set_init_prvl)

- [`Disease$set_rr()`](#method-Disease-set_rr)

- [`Disease$set_incd_prb()`](#method-Disease-set_incd_prb)

- [`Disease$set_dgns_prb()`](#method-Disease-set_dgns_prb)

- [`Disease$set_mrtl_prb()`](#method-Disease-set_mrtl_prb)

- [`Disease$del_parf_file()`](#method-Disease-del_parf_file)

- [`Disease$get_incd()`](#method-Disease-get_incd)

- [`Disease$get_dur()`](#method-Disease-get_dur)

- [`Disease$get_prvl()`](#method-Disease-get_prvl)

- [`Disease$get_ftlt()`](#method-Disease-get_ftlt)

- [`Disease$get_seed()`](#method-Disease-get_seed)

- [`Disease$get_rr()`](#method-Disease-get_rr)

- [`Disease$del_stochastic_effect()`](#method-Disease-del_stochastic_effect)

- [`Disease$get_parf()`](#method-Disease-get_parf)

- [`Disease$get_parf_filename()`](#method-Disease-get_parf_filename)

- [`Disease$harmonise_epi_tables()`](#method-Disease-harmonise_epi_tables)

- [`Disease$to_cpp()`](#method-Disease-to_cpp)

- [`Disease$print()`](#method-Disease-print)

- [`Disease$clone()`](#method-Disease-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new disease object.

#### Usage

    Disease$new(name, friendly_name, meta, notes = NA_character_, design_, RR)

#### Arguments

- `name`:

  A string with disease name.

- `friendly_name`:

  A string with disease friendly name.

- `meta`:

  A list with the disease type and other information for incidence,
  diagnosis, and mortality.

- `notes`:

  A string with any notes.

- `design_`:

  A design object with the simulation parameters.

- `RR`:

  A list of exposure objects.

#### Returns

A new \`Disease\` object.

------------------------------------------------------------------------

### Method `gen_parf_files()`

Generates PARF and stores it to disk if one doesn not exists already.

#### Usage

    Disease$gen_parf_files(
      design_ = design,
      diseases_ = diseases,
      popsize = 100,
      check = design_$sim_prm$logs,
      keep_intermediate_file = TRUE,
      bUpdateExistingDiseaseSnapshot = TRUE
    )

#### Arguments

- `design_`:

  A design object with the simulation parameters.

- `diseases_`:

  A list of Disease objects.

- `popsize`:

  The population size for each stratum.

- `check`:

  Check for NAs in parf_dt.

- `keep_intermediate_file`:

  Whether to keep the intermediate synthpop file.

- `bUpdateExistingDiseaseSnapshot`:

  bool, update existing disease PARF and snapshot files as necessary.

#### Returns

The PARF data.table if it was created, otherwise \`NULL\`.

------------------------------------------------------------------------

### Method `gen_parf()`

Read PARF file from disk. If missing, generates PARF and writes it to
disk.

#### Usage

    Disease$gen_parf(
      sp = sp,
      design_ = design,
      diseases_ = diseases,
      popsize = 100,
      check = design_$sim_prm$logs,
      keep_intermediate_file = TRUE
    )

#### Arguments

- `sp`:

  A synthpop object

- `design_`:

  A design object with the simulation parameters.

- `diseases_`:

  A list of Disease objects

- `popsize`:

  The population size for each stratum

- `check`:

  Check for NAs in parf_dt.

- `keep_intermediate_file`:

  Whether to keep the intermediate synthpop file

#### Returns

The invisible self for chaining.

------------------------------------------------------------------------

### Method `set_init_prvl()`

Set disease prevalence & diagnosis in a new col in sp\$pop.

#### Usage

    Disease$set_init_prvl(sp, design_ = design)

#### Arguments

- `sp`:

  A synthetic population.

- `design_`:

  A design object with the simulation parameters.

#### Returns

The invisible self for chaining.

------------------------------------------------------------------------

### Method `set_rr()`

Set disease incidence probability in a new col in sp\$pop.

#### Usage

    Disease$set_rr(
      sp,
      design_ = design,
      checkNAs = design_$sim_prm$logs,
      forPARF = FALSE
    )

#### Arguments

- `sp`:

  A synthetic population.

- `design_`:

  A design object with the simulation parameters.

- `checkNAs`:

  If \`TRUE\`, prints the table of NAs before they get overwritten
  with 1. Note that for some exposures, NAs are expected for certain
  levels of exposure (i.e. for active days).

- `forPARF`:

  Set TRUE when applied on the specialised forPARF SynthPop

#### Returns

The invisible self for chaining.

------------------------------------------------------------------------

### Method `set_incd_prb()`

Set disease incident probability in a new col in sp\$pop.

#### Usage

    Disease$set_incd_prb(sp, design_ = design)

#### Arguments

- `sp`:

  A synthetic population.

- `design_`:

  A design object with the simulation parameters.

#### Returns

The invisible self for chaining.

------------------------------------------------------------------------

### Method `set_dgns_prb()`

Set diagnosis probability in a new col in sp\$pop.

#### Usage

    Disease$set_dgns_prb(sp, design_ = design)

#### Arguments

- `sp`:

  A synthetic population.

- `design_`:

  A design object with the simulation parameters.

#### Returns

The invisible self for chaining.

------------------------------------------------------------------------

### Method `set_mrtl_prb()`

Set disease case fatality when relevant, in a new col in sp\$pop.

#### Usage

    Disease$set_mrtl_prb(sp, design_ = design)

#### Arguments

- `sp`:

  A synthetic population.

- `design_`:

  A design object with the simulation parameters.

#### Returns

The invisible self for chaining.

------------------------------------------------------------------------

### Method `del_parf_file()`

Deletes the PARF file from disk.

#### Usage

    Disease$del_parf_file(invert = FALSE)

#### Arguments

- `invert`:

  deletes all other disease relevant PARF file except those that are
  associated to the current settings.

#### Returns

The invisible self for chaining.

------------------------------------------------------------------------

### Method `get_incd()`

Get disease incident probability.

#### Usage

    Disease$get_incd(year_, mc_ = sp$mc_aggr, design_ = design)

#### Arguments

- `year_`:

  A vector of years to return. All if missing.

- `mc_`:

  A scalar to realise the incidence probability. All if missing. The
  median for mc\_ = 0

- `design_`:

  A design object.

#### Returns

A data.table with disease incident probabilities unless incidence type:
Universal when it returns data.table(NULL).

------------------------------------------------------------------------

### Method `get_dur()`

Get disease duration distribution parameters.

#### Usage

    Disease$get_dur(mc_ = sp$mc_aggr)

#### Arguments

- `mc_`:

  A scalar to realise the incidence probability. All if missing. The
  median for mc\_ = 0

#### Returns

A data.table with duration distribution parameters. unless incidence
type: Universal when it returns data.table(NULL).

------------------------------------------------------------------------

### Method `get_prvl()`

Get disease prevalent probability.

#### Usage

    Disease$get_prvl(year_, mc_ = sp$mc_aggr, design_ = design)

#### Arguments

- `year_`:

  A vector of years to return. All if missing.

- `mc_`:

  A scalar to realise the prevalence probability. All if missing. The
  median for mc\_ = 0

- `design_`:

  A design object.

#### Returns

A data.table with disease prevalent probabilities unless incidence type:
Universal when it returns data.table(NULL).

------------------------------------------------------------------------

### Method `get_ftlt()`

Get disease case fatality probability.

#### Usage

    Disease$get_ftlt(year_, mc_ = sp$mc_aggr, design_ = design)

#### Arguments

- `year_`:

  A vector of years to return. All if missing.

- `mc_`:

  A scalar to realise the incidence probability. All if missing. The
  median for mc\_ = 0

- `design_`:

  A design object.

#### Returns

A data.table with disease case fatality probabilities unless mortality
type: Non-fatal when it returns data.table(NULL).

------------------------------------------------------------------------

### Method `get_seed()`

Get seed for RNG.

#### Usage

    Disease$get_seed()

#### Returns

A seed for the RNG that is produced by the digest of disease name and
outcome.

------------------------------------------------------------------------

### Method `get_rr()`

Get the list of rr for all relevant exposures.

#### Usage

    Disease$get_rr()

#### Returns

A list of exposure objects.

------------------------------------------------------------------------

### Method `del_stochastic_effect()`

Deletes the stochastic effect files and indices from disk for all
relevant RR.

#### Usage

    Disease$del_stochastic_effect()

#### Returns

The invisible self for chaining.

------------------------------------------------------------------------

### Method `get_parf()`

Get the PARF by age/sex.

#### Usage

    Disease$get_parf(what)

#### Arguments

- `what`:

  Columns to return (p0, m0, or parf)

#### Returns

A data.table with PARF.

------------------------------------------------------------------------

### Method `get_parf_filename()`

Get the PARF filename.

#### Usage

    Disease$get_parf_filename()

#### Returns

A data.table with PARF.

------------------------------------------------------------------------

### Method `harmonise_epi_tables()`

Harmonises classes and levels between the synthetic population and the
incidence/prevalence/fatality tables. It saves the harmonised table to
disk, overwriting the existing one.

#### Usage

    Disease$harmonise_epi_tables(sp, verbose = FALSE)

#### Arguments

- `sp`:

  A synthetic population.

- `verbose`:

  Logical. If TRUE, print the row number of the table that is processed
  to estimate shape1 and shape2.

#### Returns

The invisible self for chaining.

------------------------------------------------------------------------

### Method `to_cpp()`

Returns a list to pass to the C++ side for Chris' parser.

#### Usage

    Disease$to_cpp(sp, design_ = design, scenario_name, scenario_suffix = "")

#### Arguments

- `sp`:

  A synthetic population.

- `design_`:

  A design object with the simulation parameters.

- `scenario_name`:

  A string with the scenario name. Currently is only used when kismet ==
  FALSE to generate new seeds for each scenario.

- `scenario_suffix`:

  the suffix to identify columns from different scenarios.

#### Returns

A list.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the simulation parameters.

#### Usage

    Disease$print()

#### Returns

The invisible self for chaining.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Disease$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

# R6 Class representing a synthetic population

A synthpop has a \`pop\` field that contains the life course of
simulants in a \`data.table\`.

## Details

To be completed...

## Public fields

- `mc`:

  The Monte Carlo iteration of the synthetic population fragment. Every
  integer generates a unique synthetic population fragment.

- `mc_aggr`:

  The Monte Carlo iteration of the synthetic population to be used when
  multiple synthetic population fragments getting aggregated. For
  instance if the synthpop consists of 2 fragments, mc_aggr will be the
  same for both, but mc will differ. It ensures correct seeds for the
  RNGs during the simulation for the RRs and the lags.

- `metadata`:

  Metadata of the synthpop.

- `pop`:

  The data.table that contains the life-course of simulants. If the file
  exists, it is loaded from disk. If it doesn't, it is first generated,
  then saved to disk, and then loaded from disk.

## Methods

### Public methods

- [`SynthPop$new()`](#method-SynthPop-new)

- [`SynthPop$update_design()`](#method-SynthPop-update_design)

- [`SynthPop$update_pop_weights()`](#method-SynthPop-update_pop_weights)

- [`SynthPop$delete_synthpop()`](#method-SynthPop-delete_synthpop)

- [`SynthPop$delete_incomplete_synthpop()`](#method-SynthPop-delete_incomplete_synthpop)

- [`SynthPop$check_integridy()`](#method-SynthPop-check_integridy)

- [`SynthPop$count_synthpop()`](#method-SynthPop-count_synthpop)

- [`SynthPop$get_checksum()`](#method-SynthPop-get_checksum)

- [`SynthPop$get_filename()`](#method-SynthPop-get_filename)

- [`SynthPop$get_design()`](#method-SynthPop-get_design)

- [`SynthPop$get_dir()`](#method-SynthPop-get_dir)

- [`SynthPop$gen_synthpop_demog()`](#method-SynthPop-gen_synthpop_demog)

- [`SynthPop$write_synthpop()`](#method-SynthPop-write_synthpop)

- [`SynthPop$get_risks()`](#method-SynthPop-get_risks)

- [`SynthPop$store_risks()`](#method-SynthPop-store_risks)

- [`SynthPop$print()`](#method-SynthPop-print)

- [`SynthPop$clone()`](#method-SynthPop-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new SynthPop object. If a synthpop file in
[`fst-package`](http://www.fstpackage.org/reference/fst-package.md)
format already exists, then the synthpop is loaded from there. Otherwise
it is generated from scratch and then saved as \`filename\` in
[`fst-package`](http://www.fstpackage.org/reference/fst-package.md)
format. Two additional files are saved for each 'synthpop'. A metadata
file, and an index file.

#### Usage

    SynthPop$new(mc_, design_)

#### Arguments

- `mc_`:

  The Monte Carlo iteration of the synthetic population. Each integer
  generates a unique synthetic population. If \`mc = 0\` an object with
  an empty synthpop is initiated.

- `design_`:

  A
  [`Design`](https://christk.github.io/IMPACTncd_Japan/reference/Design.md)
  object.

- `synthpop_dir_`:

  The directory where 'SynthPop' objects are stored. The synthpop file
  in [`fst-package`](http://www.fstpackage.org/reference/fst-package.md)
  format. If \`filename\` already exists, then the synthpop is loaded
  from there. Otherwise it is generated from scratch and then saved as
  \`filename\` in
  [`fst-package`](http://www.fstpackage.org/reference/fst-package.md)
  format. Two additional files are saved for each 'synthpop'. A metadata
  file, and an index file.

#### Returns

A new \`SynthPop\` object.

#### Examples

    design <- Design$new("./validation/design_for_trends_validation.yaml")
    POP$write_synthpop(1:6, design)
    POP <- SynthPop$new(4L, design)
    POP$print()
    POP$count_synthpop()

    POP$delete_synthpop(1L)
    POP$delete_synthpop(5:6)
    POP$get_filename()

------------------------------------------------------------------------

### Method `update_design()`

Updates the Design object that is stored in the SynthPop object.

#### Usage

    SynthPop$update_design(design_ = design)

#### Arguments

- `design_`:

  A design object with the simulation parameters.

#### Returns

The invisible self for chaining.

------------------------------------------------------------------------

### Method `update_pop_weights()`

Updates the wt_immrtl to account for mortality in baseline scenario.

#### Usage

    SynthPop$update_pop_weights(scenario_nam = "sc0")

#### Arguments

- `scenario_nam`:

  The scenario name. Logic is different if "sc0".

#### Returns

The invisible self for chaining.

------------------------------------------------------------------------

### Method `delete_synthpop()`

Delete (all) synthpop files in the synthpop directory.

#### Usage

    SynthPop$delete_synthpop(mc_, check_checksum = TRUE, invert = FALSE)

#### Arguments

- `mc_`:

  If \`mc\_ = NULL\`, delete all files in the synthpop directory. If
  \`mc\_\` is an integer vector delete the specific synthpop files
  including the metadata and index files.

- `check_checksum`:

  If \`TRUE\` only delete files with the same checksum as the synthpop.
  Only relevant when \`mc\_ = NULL\`.

- `invert`:

  If \`TRUE\` (default is \`FALSE\`) keeps files with the same checksum
  as the synthpop and deletes all other synthpops. Only relevant when
  \`mc\_ = NULL\` and \`check_checksum = TRUE\`.

#### Returns

The invisible \`SynthPop\` object.

------------------------------------------------------------------------

### Method `delete_incomplete_synthpop()`

Check that every synthpop file has a metafile and an index file. Delete
any orphan files.

#### Usage

    SynthPop$delete_incomplete_synthpop(check_checksum = TRUE)

#### Arguments

- `check_checksum`:

  If \`TRUE\` only delete incomplete group files with the same checksum
  as the synthpop.

#### Returns

The invisible \`SynthPop\` object.

------------------------------------------------------------------------

### Method `check_integridy()`

Check the integrity of (and optionally delete) .fst files by checking
their metadata are readable.

#### Usage

    SynthPop$check_integridy(remove_malformed = FALSE, check_checksum = TRUE)

#### Arguments

- `remove_malformed`:

  If \`TRUE\`, delete all malformed .fst files and their associated
  files.

- `check_checksum`:

  If \`TRUE\` only check files with the same checksum as the synthpop.

#### Returns

The invisible \`SynthPop\` object.

------------------------------------------------------------------------

### Method `count_synthpop()`

Count the synthpop files in a directory. It includes files without
metafiles and index files.

#### Usage

    SynthPop$count_synthpop()

#### Returns

The invisible \`SynthPop\` object.

------------------------------------------------------------------------

### Method `get_checksum()`

Get the synthpop checksum.

#### Usage

    SynthPop$get_checksum()

#### Arguments

- `x`:

  One of "all", "synthpop" or "metafile". Can be abbreviated.

#### Returns

The invisible \`SynthPop\` object.

------------------------------------------------------------------------

### Method `get_filename()`

Get the synthpop file paths.

#### Usage

    SynthPop$get_filename(x = c("all", "synthpop", "metafile"))

#### Arguments

- `x`:

  One of "all", "synthpop" or "metafile". Can be abbreviated.

#### Returns

The invisible \`SynthPop\` object.

------------------------------------------------------------------------

### Method `get_design()`

Get the synthpop design.

#### Usage

    SynthPop$get_design()

#### Returns

The invisible \`SynthPop\` object.

------------------------------------------------------------------------

### Method `get_dir()`

Get the synthpop dir.

#### Usage

    SynthPop$get_dir()

#### Returns

The invisible \`SynthPop\` object.

------------------------------------------------------------------------

### Method `gen_synthpop_demog()`

Generate synthpop sociodemographics, random sample of the population.

#### Usage

    SynthPop$gen_synthpop_demog(design_, month = "July")

#### Arguments

- `design_`:

  A Design object,

- `month`:

  April or July are accepted. Use July for mid-year population
  estimates.

#### Returns

An invisible \`data.table\` with sociodemographic information.

------------------------------------------------------------------------

### Method `write_synthpop()`

Generate synthpop files in parallel, using foreach, and writes them to
disk. It skips files that are already on disk. Note: the backend for
foreach needs to be initialised before calling the function.

#### Usage

    SynthPop$write_synthpop(mc_)

#### Arguments

- `mc_`:

  An integer vector for the Monte Carlo iteration of the synthetic
  population. Each integer generates a unique synthetic population.

#### Returns

The invisible \`SynthPop\` object.

------------------------------------------------------------------------

### Method `get_risks()`

Get the risks for all individuals in a synthetic population for a
disease.

#### Usage

    SynthPop$get_risks(disease_nam)

#### Arguments

- `disease_nam`:

  The disease that the risks will be returned.

#### Returns

A data.table with columns for pid, year, and all associated risks if
disease_nam is specified. Else a list of data.tables for all diseases.

------------------------------------------------------------------------

### Method `store_risks()`

Stores the disease risks for all individuals in a synthetic population
in a private list.

#### Usage

    SynthPop$store_risks(disease_nam)

#### Arguments

- `disease_nam`:

  The disease that the risks will be stored.

#### Returns

The invisible self for chaining.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Prints the synthpop object metadata.

#### Usage

    SynthPop$print()

#### Returns

The invisible \`SynthPop\` object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    SynthPop$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
## ------------------------------------------------
## Method `SynthPop$new`
## ------------------------------------------------

design <- Design$new("./validation/design_for_trends_validation.yaml")
#> Error in base::normalizePath(sim_prm, mustWork = TRUE): path[1]="./validation/design_for_trends_validation.yaml": No such file or directory
POP$write_synthpop(1:6, design)
#> Error: object 'POP' not found
POP <- SynthPop$new(4L, design)
#> Error: object 'design' not found
POP$print()
#> Error: object 'POP' not found
POP$count_synthpop()
#> Error: object 'POP' not found

POP$delete_synthpop(1L)
#> Error: object 'POP' not found
POP$delete_synthpop(5:6)
#> Error: object 'POP' not found
POP$get_filename()
#> Error: object 'POP' not found
```

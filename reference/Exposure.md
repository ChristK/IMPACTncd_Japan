# R6 Class representing an exposure

An exposure object has the RR between an exposure and a disease.

## Details

To be completed...

## Public fields

- `name`:

  The name of the exposure.

- `outcome`:

  The name of the outcome the exposure influences.

- `lag`:

  Disease lag.

- `distribution`:

  The distribution to be used for Monte Carlo. Currently lognormal and
  normal are supported.

- `source`:

  Citation info for the effect size.

- `notes`:

  Any notes regarding the exposure -\> outcome relation.

## Methods

### Public methods

- [`Exposure$new()`](#method-Exposure-new)

- [`Exposure$gen_stochastic_effect()`](#method-Exposure-gen_stochastic_effect)

- [`Exposure$del_stochastic_effect()`](#method-Exposure-del_stochastic_effect)

- [`Exposure$get_rr()`](#method-Exposure-get_rr)

- [`Exposure$clear_cache()`](#method-Exposure-clear_cache)

- [`Exposure$xps_to_rr()`](#method-Exposure-xps_to_rr)

- [`Exposure$validate_rr()`](#method-Exposure-validate_rr)

- [`Exposure$get_input_rr()`](#method-Exposure-get_input_rr)

- [`Exposure$get_metadata()`](#method-Exposure-get_metadata)

- [`Exposure$get_seed()`](#method-Exposure-get_seed)

- [`Exposure$write_xps_tmplte_file()`](#method-Exposure-write_xps_tmplte_file)

- [`Exposure$convert_from_old_format()`](#method-Exposure-convert_from_old_format)

- [`Exposure$get_lag()`](#method-Exposure-get_lag)

- [`Exposure$get_ideal_xps_lvl()`](#method-Exposure-get_ideal_xps_lvl)

- [`Exposure$get_name()`](#method-Exposure-get_name)

- [`Exposure$print()`](#method-Exposure-print)

- [`Exposure$clone()`](#method-Exposure-clone)

------------------------------------------------------------------------

### Method `new()`

Reads exposure parameter from file and creates a new exposure object..

#### Usage

    Exposure$new(sRelativeRiskByPopulationSubsetForExposureFilePath, design)

#### Arguments

- `sRelativeRiskByPopulationSubsetForExposureFilePath`:

  string, path to .csvy file detailing relative risk (RR) by population
  subset (age, sex, maybe decile 'Index of Multiple Deprivation' DIMD)
  for a specific exposure. File header may contain other exposure
  parameters.

- `design`:

  A design object with the simulation parameters.

#### Returns

An \`Exposure\` object.

#### Examples

    af_stroke$read_xps_prm("./inputs/RR/af_stroke.csvy", design)

------------------------------------------------------------------------

### Method `gen_stochastic_effect()`

Generates and write to disk the stochastic effect.

#### Usage

    Exposure$gen_stochastic_effect(
      design_ = design,
      overwrite = FALSE,
      smooth,
      ...
    )

#### Arguments

- `design_`:

  A design object with the simulation parameters.

- `overwrite`:

  If TRUE overwrite the files. Else if files exist they are not
  regenerated.

- `smooth`:

  If true applies loess smoothing to the input relative risks

- `...`:

  Further arguments to be passed to \`loess\`, usually span = 0.7,
  degree = 1

#### Returns

An \`Exposure\` object.

#### Examples

    af_stroke <- Exposure$new("af", "stroke")
    af_stroke$read_xps_prm("./inputs/RR/af_stroke.csvy", design)
    af_stroke$gen_stochastic_effect(design, TRUE)

------------------------------------------------------------------------

### Method `del_stochastic_effect()`

Deletes the stochastic effect file and index from disk.

#### Usage

    Exposure$del_stochastic_effect(invert = FALSE)

#### Arguments

- `invert`:

  If TRUE keeps the file with the current checksum and deletes all other
  files for this exposure~outcome.

#### Returns

The invisible self for chaining.

------------------------------------------------------------------------

### Method `get_rr()`

Get relative risks from disk.

#### Usage

    Exposure$get_rr(mc, design_ = design, drop = FALSE, plot_rr = FALSE)

#### Arguments

- `mc`:

  An integer that signifies the Monte Carlo iteration.

- `design_`:

  A design object with the simulation parameters.

- `drop`:

  If \`TRUE\` returns a scalar numeric of the RR if the RR is common for
  all age groups and both sexes. Otherwise, a data.table.

- `plot_rr`:

  If TRUE, plots the relative risk

#### Returns

A data.table with the stochastic relative risks, if stochastic = TRUE or
mc \> 0; else, the deterministic relative risks.

------------------------------------------------------------------------

### Method `clear_cache()`

Clear the cache for get_rr.

#### Usage

    Exposure$clear_cache()

#### Returns

The \`Exposure\` object.

------------------------------------------------------------------------

### Method `xps_to_rr()`

Apply the RR in a new column in sp\$pop based on the exposure level. I
the case of smok_quit_yrs it modifies the risk of smok_cig or packyrs
and no new column is created.

#### Usage

    Exposure$xps_to_rr(
      sp_,
      design_,
      checkNAs = design_$sim_prm$logs,
      forPARF = FALSE
    )

#### Arguments

- `sp_`:

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

### Method `validate_rr()`

Get relative risks from disk.

#### Usage

    Exposure$validate_rr()

#### Returns

A plot with the input and stochastic relative risks.

------------------------------------------------------------------------

### Method `get_input_rr()`

Get original input relative risks by age and sex.

#### Usage

    Exposure$get_input_rr()

#### Returns

A copied data.table with the original relative risks.

------------------------------------------------------------------------

### Method `get_metadata()`

Get original metadata.

#### Usage

    Exposure$get_metadata()

#### Returns

A list with the original metadata.

------------------------------------------------------------------------

### Method `get_seed()`

Get seed for RNG.

#### Usage

    Exposure$get_seed()

#### Returns

A seed for the RNG that is produced by the digest of exposure name and
outcome.

------------------------------------------------------------------------

### Method `write_xps_tmplte_file()`

Writes a template exposure file to disk.

#### Usage

    Exposure$write_xps_tmplte_file(file_path = "./inputs/RR/template.csvy")

#### Arguments

- `file_path`:

  Path including file name and .csvy extension to write the file with
  placeholder exposure parameters.

#### Returns

The \`Exposure\` object.

------------------------------------------------------------------------

### Method `convert_from_old_format()`

Convert the old format .csv to the new format .csvy.

#### Usage

    Exposure$convert_from_old_format(
      old_file,
      metadata,
      file_path,
      estimates = NULL,
      second_estimate_is_p = FALSE
    )

#### Arguments

- `old_file`:

  Path to the old format .csv file with the RR.

- `metadata`:

  List with the metadata information.

- `file_path`:

  Path including file name and .csvy extension to write the new file.

- `estimates`:

  Only used when old_file is missing. A vector of length 2 with the 1st
  element being the point estimate and the 2nd one of the CI.

- `second_estimate_is_p`:

  If \`TRUE\` and the \`estimates\` not \`NULL\` then the second element
  of \`estimates\` is interpreted like a p-value for ratio rather than a
  CI.

#### Returns

The \`Exposure\` object.

------------------------------------------------------------------------

### Method `get_lag()`

Get exposure lag.

#### Usage

    Exposure$get_lag(mc_)

#### Arguments

- `mc_`:

  A vector of Monte Carlo iterations. If missing or 0 return median (=
  mean).

#### Returns

An integer vector with exposure~disease lag.

------------------------------------------------------------------------

### Method `get_ideal_xps_lvl()`

Get ideal exposure level.

#### Usage

    Exposure$get_ideal_xps_lvl(mc_)

#### Arguments

- `mc_`:

  A vector of Monte Carlo iterations. If missing or 0 return the user
  input lag.

#### Returns

An integer vector with exposure~disease lag.

------------------------------------------------------------------------

### Method `get_name()`

Get name of the object.

#### Usage

    Exposure$get_name()

#### Returns

An string.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the simulation parameters.

#### Usage

    Exposure$print()

#### Returns

The \`Exposure\` object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Exposure$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
## ------------------------------------------------
## Method `Exposure$new`
## ------------------------------------------------

af_stroke$read_xps_prm("./inputs/RR/af_stroke.csvy", design)
#> Error: object 'af_stroke' not found

## ------------------------------------------------
## Method `Exposure$gen_stochastic_effect`
## ------------------------------------------------

af_stroke <- Exposure$new("af", "stroke")
#> Error in normalizePath(sRelativeRiskByPopulationSubsetForExposureFilePath,     mustWork = TRUE): path[1]="af": No such file or directory
af_stroke$read_xps_prm("./inputs/RR/af_stroke.csvy", design)
#> Error: object 'af_stroke' not found
af_stroke$gen_stochastic_effect(design, TRUE)
#> Error: object 'af_stroke' not found
```

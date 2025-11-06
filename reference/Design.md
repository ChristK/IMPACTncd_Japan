# R6 Class representing a simulation design

A design has a sim_prm list that holds the simulation parameters.

## Details

To be completed...

## Public fields

- `sim_prm`:

  The simulation parameters.

## Methods

### Public methods

- [`Design$new()`](#method-Design-new)

- [`Design$save_to_disk()`](#method-Design-save_to_disk)

- [`Design$update_fromGUI()`](#method-Design-update_fromGUI)

- [`Design$print()`](#method-Design-print)

- [`Design$clone()`](#method-Design-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new design object.

#### Usage

    Design$new(sim_prm)

#### Arguments

- `sim_prm`:

  Either a path to a yaml file or a list with appropriate format.

#### Returns

A new \`Design\` object.

#### Examples

    design <- Design$new("./validation/design_for_trends_validation.yaml")

------------------------------------------------------------------------

### Method `save_to_disk()`

Create a new design object.

#### Usage

    Design$save_to_disk(path)

#### Arguments

- `path`:

  Path including file name and extension to save a yaml file with the
  simulation parameters.

#### Returns

The \`Design\` object.

------------------------------------------------------------------------

### Method `update_fromGUI()`

Updates the design object from GUI.

#### Usage

    Design$update_fromGUI(GUI_prm)

#### Arguments

- `GUI_prm`:

  A GUI parameter object.

#### Returns

The \`Design\` object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the simulation parameters.

#### Usage

    Design$print()

#### Returns

The \`Design\` object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Design$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
## ------------------------------------------------
## Method `Design$new`
## ------------------------------------------------

design <- Design$new("./validation/design_for_trends_validation.yaml")
#> Error in base::normalizePath(sim_prm, mustWork = TRUE): path[1]="./validation/design_for_trends_validation.yaml": No such file or directory
```

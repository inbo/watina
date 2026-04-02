# Compute the ionic ratio

Calculate the ionic ratio of a water sample based on the output of
[`get_chem`](https://inbo.github.io/watina/reference/get_chem.md) and
add it to the input data as a new column `ir`.

## Usage

``` r
calculate_ir(data)
```

## Arguments

- data:

  Output of the `get_chem` function (hydrochemical data retrieved from
  Watina) or any dataset with at least the columns `loc_code`,
  `chem_variable` (with at least concentrations for Ca and Cl), `value`,
  `unit` and `date`. It can be a lazy object as well as a local object
  `(get_chem(collect = TRUE))`.

## Value

A tibble similar to the input `data` but with an extra column `ir` with
the calculated ionic ratio.

## Details

Depending on the units of the Ca and Cl concentrations, the ionic ratio
gets calculated as follows:

1.  if the Ca and Cl concentrations are in meq/l: ionic ratio = \[Ca2+\]
    / (\[Ca2+\] + \[Cl-\])

2.  if the Ca and Cl concentrations are in mg/l: ionic ratio =
    ((Ca_mg\*2)/40.078) / (((Ca_mg\*2)/40.078) + (Cl_mg/35.453))

The result is the ionic ratio `ir` without units (0-1).

## Examples

``` r
if (FALSE) { # \dontrun{
watina <- connect_watina()

# get the chemical data
mydata <-
  get_locs(watina, area_codes = "ZWA") %>%
  get_chem(watina, "1/1/2019") %>%
  collect()

# compute ionic ratio and add as new field
mydata_with_ir <- calculate_ir(mydata)
} # }
```

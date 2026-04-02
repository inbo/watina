# Evaluate hydrochemical data per location

For a dataset as returned by
[`get_chem`](https://inbo.github.io/watina/reference/get_chem.md),
return summary statistics (data availability and/or numeric properties)
of the specified hydrochemical variables, for each location.

## Usage

``` r
eval_chem(
  data,
  chem_var = c("P-PO4", "N-NO3", "N-NO2", "N-NH4", "HCO3", "SO4", "Cl", "Na", "K", "Ca",
    "Mg", "Fe", "Mn", "Si", "Al", "CondF", "CondL", "pHF", "pHL"),
  type = c("avail", "num", "both"),
  uniformity_test = FALSE
)
```

## Arguments

- data:

  An object returned by
  [`get_chem`](https://inbo.github.io/watina/reference/get_chem.md).

- chem_var:

  A character vector to select chemical variables for which statistics
  will be computed. To specify chemical variables, use the codes from
  the column `chem_variable` in `data`.

- type:

  A string defining the requested type of summary statistics. See
  section 'Value'. Either:

  - `"avail"`: availability statistics (the default);

  - `"num"`: numeric summary statistics;

  - `"both"`: both types will be returned.

- uniformity_test:

  Should the availability statistic `pval_uniform_totalspan` be added
  (see section 'Value')? Defaults to `FALSE` as this takes much more
  time to calculate than everything else.

## Value

A tibble with variables `loc_code` (see
[`get_locs`](https://inbo.github.io/watina/reference/get_locs.md)),
`chem_variable` (character; see Details) and summary statistics (*at the
level of `loc_code` x `chem_variable`*) depending on the state of `type`
(`type = "both"` combines both cases):

With `type = "avail"` (the default):

- `nryears`: number of calendar years for which the chemical variable is
  available (on one date at least)

- `nrdates`: number of dates at which the chemical variable is available

- `firstdate`: earliest date at which the chemical variable is available

- `lastdate`: latest date at which the chemical variable is available

- `timespan_years`: duration as years from the first calendar year (year
  of `firstdate`) to the last calendar year (year of `lastdate`) with
  data available

- `timespan_totalspan_ratio`: the ratio of `timespan_years` to the
  'total span', which is the duration as years from the first to the
  last calendar year *for the whole of **`data`***.

- `nryears_totalspan_ratio`: the ratio of `nryears` to the 'total span'

- `pval_uniform_totalspan`: Only returned with `uniformity_test = TRUE`.
  p-value of an exact, one-sample two-sided Kolmogorov-Smirnov test for
  the discrete uniform distribution of the calendar years *with data of
  the chemical variable available* within the series of consecutive
  calendar years defined by the 'total span' (see above). The smaller
  the p-value, the less uniform the years are spread within the total
  span. A perfectly uniform spread results in a p-value of 1.

With `type = "num"`: summary statistics based on the chemical values,
i.e. excluding the 'combined' level:

- `val_min`: minimum value

- `val_pct10`, `val_pct25`, `val_pct50`, `val_pct75`, `val_pct90`:
  percentiles

- `val_max`: maximum value

- `val_range`: range

- `val_mean`: mean

- `val_geometric_mean`: geometric mean. Only calculated when all values
  are strictly positive.

- `unit`

- `prop_below_loq`: the proportion of measurements below the limit of
  quantification (as derived from `below_loq == TRUE`), i.e. relative to
  the total number of measurements *for which `below_loq` is not `NA`*.
  This statistic neglects measurements with value `NA` for `below_loq`!
  If all measurements have value `NA` for `below_loq`, this statistic is
  set to `NA`.

## Details

For the availability statistics, an extra level "`combined`" is added in
the column `chem_variable` whenever the arguments `data` and `chem_var`
imply more than one chemical variable to be investigated. This
'combined' level defines data availability for a water sample as the
availability of data for **all** corresponding chemical variables.

## See also

Other functions to evaluate locations:
[`eval_xg3_avail()`](https://inbo.github.io/watina/reference/eval_xg3_avail.md),
[`eval_xg3_series()`](https://inbo.github.io/watina/reference/eval_xg3_series.md)

## Examples

``` r
if (FALSE) { # \dontrun{
watina <- connect_watina()
library(dplyr)
mylocs <- get_locs(watina, area_codes = "ZWA")
mydata <-
  mylocs %>%
  get_chem(watina, "1/1/2010")
mydata %>% arrange(loc_code, date, chem_variable)
mydata %>%
  pull(date) %>%
  lubridate::year(.) %>%
  (function(x) c(firstyear = min(x), lastyear = max(x)))
mydata %>%
  eval_chem(chem_var = c("P-PO4", "N-NO3", "N-NO2", "N-NH4")) %>%
  arrange(desc(loc_code))
mydata %>%
  eval_chem(
    chem_var = c("P-PO4", "N-NO3", "N-NO2", "N-NH4"),
    type = "both"
  ) %>%
  arrange(desc(loc_code)) %>%
  as.data.frame() %>%
  head(10)
mydata %>%
  eval_chem(
    chem_var = c("P-PO4", "N-NO3", "N-NO2", "N-NH4"),
    uniformity_test = TRUE
  ) %>%
  arrange(desc(loc_code)) %>%
  select(loc_code, chem_variable, pval_uniform_totalspan)
# Disconnect:
dbDisconnect(watina)
} # }
```

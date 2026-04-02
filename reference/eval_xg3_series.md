# Identify and evaluate XG3 series per location

For a dataset as returned by
[`get_xg3`](https://inbo.github.io/watina/reference/get_xg3.md),
determine for each location the available multi-year XG3 series and
calculate summary statistics for each series. Note that 'years' in this
context always refers to hydroyears.

## Usage

``` r
eval_xg3_series(data, xg3_type = c("L", "H", "V"), max_gap, min_dur)
```

## Arguments

- data:

  An object returned by
  [`get_xg3`](https://inbo.github.io/watina/reference/get_xg3.md).

- xg3_type:

  Character vector of length 1, 2 or 3. Defines the types of XG3 which
  are taken from `data`. Specifies the 'X' in 'XG3': either `"L"`, `"H"`
  and/or `"V"`. Defaults to `"L"`.

- max_gap:

  A positive integer (can be zero). It is part of what the user defines
  as 'an XG3 series': the maximum allowed time gap between two
  consecutive XG3 values in a series, expressed as the number of years
  without XG3 value.

- min_dur:

  A strictly positive integer. It is part of what the user defines as
  'an XG3 series': the minimum required duration of an XG3 series, i.e.
  the time (expressed as years) from the first to the last year of the
  XG3 series.

## Value

A tibble with variables:

- `loc_code`: see
  [`get_locs`](https://inbo.github.io/watina/reference/get_locs.md)

- `xg3_variable`: character; see Details

- `series`: series ID, unique within `loc_code`

- `ser_length`: series duration (as years), i.e. from first to last year

- `ser_nryears`: number of years in the series for which the XG3
  variable is available

- `ser_rel_nryears`: the fraction `ser_nryears / ser_length`,

- `ser_firstyear`: first year in the series with XG3 variable

- `ser_lastyear`: last year in the series with XG3 variable

- `ser_pval_uniform`: p-value of an exact, one-sample two-sided
  Kolmogorov-Smirnov test for the discrete uniform distribution of the
  member years within the XG3 series. The smaller the p-value, the less
  uniform the member years are spread within a series. A perfectly
  uniform spread results in a p-value of 1. Only with larger values of
  `max_gap` this p-value can get low.

- Summary statistics based on the XG3 values, i.e. excluding the
  'combined' series:

  - `ser_mean`: mean XG3 value of the series, as meters.

  - `ser_sd`: standard deviation of the XG3 values of the series (an
    estimate of the superpopulation's standard deviation). As meters.

  - `ser_se_6y`: estimated standard error of the mean XG3 for a six-year
    period, applying finite population correction (i.e. for design-based
    estimation of this mean). Hence, `ser_se_6y` is zero when a series
    has no missing years. As meters. For series shorter than six years,
    the estimation is still regarding a six-year period, assuming the
    same sampling variance as in the short series.

  - `ser_rel_sd_lcl`: relative standard deviation of XG3 values, i.e.
    `ser_sd / ser_mean`. **This value is only calculated for
    `vert_crs = "local"`.**

  - `ser_rel_se_6y_lcl`: relative standard error of the mean XG3 for a
    six-year period, i.e. `ser_se_6y / ser_mean`. **This value is only
    calculated for `vert_crs = "local"`.**

## Details

An XG3 series is a location-specific, multi-year series of LG3, HG3
and/or VG3 variables and is user-restricted by `max_gap` (maximum
allowed number of empty years between 'series member' years) and
`min_dur` (minimum required length of the series). Further, given a
dataset of XG3 values per year and location, XG3 series are always
constructed *as long as possible* given the aforementioned restrictions.
For one location and XG3 variable, more than one such XG3 series may be
available, which implies that those XG3 series are separated by more
years than the value of `max_gap`.

The function returns summary statistics for the XG3 series that are
available in the dataset. The XG3 series of each site are numbered as
'prefix_series1', 'prefix_series2' with the prefix being the value of
`xg3_variable`.

The column `xg3_variable` in the resulting tibble stands for the XG3
type + the vertical CRS (see
[`get_xg3`](https://inbo.github.io/watina/reference/get_xg3.md)) to
which a series belongs. `xg3_variable` is restricted to the requested
XG3 types (LG3, HG3 and/or VG3) via the `xg3_type` argument, but adds an
extra level "`combined`" whenever the combination of `data` (which may
have both vertical CRSes) and `xg3_type` implies more than one requested
variable. This 'combined' level defines an XG3 series as an XG3 series
where each 'member' year has **all** selected XG3 variables available.

## See also

[`extract_xg3_series`](https://inbo.github.io/watina/reference/extract_xg3_series.md)

Other functions to evaluate locations:
[`eval_chem()`](https://inbo.github.io/watina/reference/eval_chem.md),
[`eval_xg3_avail()`](https://inbo.github.io/watina/reference/eval_xg3_avail.md)

## Examples

``` r
if (FALSE) { # \dontrun{
watina <- connect_watina()
library(dplyr)
mylocs <- get_locs(watina, area_codes = "KAL")
mydata <-
  mylocs %>%
  get_xg3(watina, 1900)
mydata %>% arrange(loc_code, hydroyear)
mydata %>%
  eval_xg3_series(
    xg3_type = c("L", "V"),
    max_gap = 2,
    min_dur = 5
  )
# Disconnect:
dbDisconnect(watina)
} # }
```

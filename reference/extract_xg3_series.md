# Identify XG3 series per location

For a dataset as returned by
[`get_xg3`](https://inbo.github.io/watina/reference/get_xg3.md),
determine for each location the available multi-year XG3 series. The
function is called by
[`eval_xg3_series`](https://inbo.github.io/watina/reference/eval_xg3_series.md).
Contrary to
[`eval_xg3_series`](https://inbo.github.io/watina/reference/eval_xg3_series.md),
it lists the member years of each series as separate lines. Note that
'years' in this context always refers to hydroyears.

## Usage

``` r
extract_xg3_series(data, xg3_type = c("L", "H", "V"), max_gap, min_dur)
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

- `hydroyear`: each member year of a series is listed separately

- `series`: series ID, unique within `loc_code`

- `series_length`: series duration, i.e. from first to last year

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

The function returns the available XG3 series in the dataset for each
site and XG3 variable, and numbers them for each site as
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

[`eval_xg3_series`](https://inbo.github.io/watina/reference/eval_xg3_series.md)

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
  extract_xg3_series(
    xg3_type = c("L", "V"),
    max_gap = 2,
    min_dur = 5
  )
# Disconnect:
dbDisconnect(watina)
} # }
```

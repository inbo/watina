# Evaluate the availability of XG3 values per location

For a dataset as returned by
[`get_xg3`](https://inbo.github.io/watina/reference/get_xg3.md), return
for each location the first and last (hydro)year and the number of
(hydro)years with available XG3 values of the specified type(s) (LG3,
HG3 and/or VG3).

## Usage

``` r
eval_xg3_avail(data, xg3_type = c("L", "H", "V"))
```

## Arguments

- data:

  An object returned by
  [`get_xg3`](https://inbo.github.io/watina/reference/get_xg3.md).

- xg3_type:

  Character vector of length 1, 2 or 3. Defines the types of XG3 which
  are taken from `data`. Specifies the 'X' in 'XG3': either `"L"`, `"H"`
  and/or `"V"`. Defaults to `"L"`.

## Value

A tibble with variables `loc_code` (see
[`get_locs`](https://inbo.github.io/watina/reference/get_locs.md)),
`xg3_variable` (character; see Details), `nryears`, `firstyear` and
`lastyear`.

## Details

The column `xg3_variable` in the resulting tibble stands for the XG3
type + the vertical CRS (see
[`get_xg3`](https://inbo.github.io/watina/reference/get_xg3.md)).
`xg3_variable` is restricted to the requested XG3 types (LG3, HG3 and/or
VG3) via the `xg3_type` argument, but adds an extra level "`combined`"
whenever the combination of `data` (which may have both vertical CRSes)
and `xg3_type` implies more than one requested variable. The
"`combined`" level evaluates the combined presence of all selected XG3
variables at each location.

## See also

Other functions to evaluate locations:
[`eval_chem()`](https://inbo.github.io/watina/reference/eval_chem.md),
[`eval_xg3_series()`](https://inbo.github.io/watina/reference/eval_xg3_series.md)

## Examples

``` r
if (FALSE) { # \dontrun{
watina <- connect_watina()
library(dplyr)
mylocs <- get_locs(watina, area_codes = "KAL")
mydata <-
  mylocs %>%
  get_xg3(watina, 2014)
mydata %>% arrange(loc_code, hydroyear)
eval_xg3_avail(mydata, xg3_type = c("L", "V"))
# Disconnect:
dbDisconnect(watina)
} # }
```

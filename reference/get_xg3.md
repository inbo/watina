# Get XG3 values from the data warehouse

Returns XG3 values from the *Watina* data warehouse, either as a lazy
object or as a local tibble. The values must belong to selected
locations and to a specified timeframe.

## Usage

``` r
get_xg3(
  locs,
  con,
  startyear,
  endyear = year(now()) - 1,
  vert_crs = c("local", "ostend", "both"),
  truncated = TRUE,
  with_estimated = TRUE,
  collect = FALSE
)
```

## Arguments

- locs:

  A `tbl_lazy` object or a data frame, with at least a column `loc_code`
  that defines the locations for which values are to be returned.
  Typically, this will be the object returned by
  [`get_locs`](https://inbo.github.io/watina/reference/get_locs.md).

- con:

  A `DBIConnection` object to Watina. See
  [`connect_watina`](https://inbo.github.io/watina/reference/connect_watina.md)
  to generate one.

- startyear:

  First hydroyear of the timeframe.

- endyear:

  Last hydroyear of the timeframe.

- vert_crs:

  A string, defining the 1-dimensional vertical coordinate reference
  system (CRS) of the XG3 water levels. Either `"local"` (the default,
  i.e. returned values are relative to soil surface level, with positive
  values = above soil surface), or `"ostend"` (values are from the CRS
  [Ostend
  height](http://crs.bkg.bund.de/crseu/crs/eu-description.php?crs_id=Y0JFX09PU1QrJTJGK1VOQ09S)
  (EPSG [5710](https://epsg.io/5710)), also known as 'TAW' or 'DNG'), or
  `"both"`, where the values for both CRS options are returned. The
  units are always meters.

- truncated:

  Logical. If `TRUE` (the default), the XG3 values are calculated after
  having set the underlying water level measurements that are above soil
  surface level to the soil surface level itself (which is zero in the
  case of the local CRS).

- with_estimated:

  Logical. If `TRUE` (the default), the XG3 values calculations also use
  estimated (i.e. non-measured) water level data that are available in
  the data warehouse.

- collect:

  Should the data be retrieved as a local tibble? If `FALSE` (the
  default), a `tbl_lazy` object is returned (lazy query). Hence the
  result can be further built upon before retrieving data with
  [`collect()`](https://dplyr.tidyverse.org/reference/compute.html).

## Value

By default, a `tbl_lazy` object. With `collect = TRUE`, a local
[`tibble`](https://tibble.tidyverse.org/reference/tibble.html) is
returned.

(TO BE ADDED: Explanation on the variable names of the returned object)

The suffix of the XG3 variables is either "`_lcl`" for
`vert_crs = "local"` or "`_ost`" for `vert_crs = "ostend"`.

## Details

The timeframe is a selection interval between a given first and last
hydroyear.

Note: the arguments `truncated` and `with_estimated` are currently not
used. Currently, non-truncated values are returned, with usage of
estimated values.

(TO BE ADDED: What are XG3 values? What is a hydroyear? Why truncate,
and why truncate by default? When to choose which `vert_crs`?)

## Note

Up to and including `watina 0.3.0`, the result was sorted according to
`loc_code` and `hydroyear`, both for the lazy query and the collected
result. Later versions avoid sorting in case of a lazy result, because
otherwise, when using the result inside another lazy query, this led to
'ORDER BY' constructs in SQL subqueries, which must be avoided. If you
like to print the lazy object in a sorted manner, you must add
`%>% arrange(...)` yourself.

## See also

Other functions to query the data warehouse:
[`get_chem()`](https://inbo.github.io/watina/reference/get_chem.md),
[`get_locs()`](https://inbo.github.io/watina/reference/get_locs.md)

## Examples

``` r
if (FALSE) { # \dontrun{
watina <- connect_watina()
library(dplyr)
mylocs <- get_locs(watina, area_codes = "KAL")
mylocs %>%
  get_xg3(watina, 2010) %>%
  arrange(loc_code, hydroyear)
mylocs %>% get_xg3(watina, 2010, collect = TRUE)
mylocs %>%
  get_xg3(watina, 2010, vert_crs = "ostend") %>%
  arrange(loc_code, hydroyear)

# joining results to mylocs:
mylocs %>%
  get_xg3(watina, 2010) %>%
  left_join(
    mylocs %>%
      select(-loc_wid),
    .
  ) %>%
  collect() %>%
  arrange(loc_code, hydroyear)

# Disconnect:
dbDisconnect(watina)
} # }
```

# Get locations from the data warehouse

Returns locations (and optionally, observation wells) from the *Watina*
data warehouse that meet several criteria, either as a lazy object or as
a local tibble. Criteria refer to spatial or non-spatial physical
attributes of the location or the location's observation wells.
Essential metadata are included in the result.

## Usage

``` r
get_locs(
  con,
  filterdepth_range = c(0, 3),
  filterdepth_guess = FALSE,
  filterdepth_na = FALSE,
  obswells = FALSE,
  obswell_aggr = c("latest", "latest_fd", "latest_sso", "mean"),
  mask = NULL,
  join_mask = FALSE,
  buffer = 10,
  bbox = NULL,
  area_codes = NULL,
  loc_type = c("P", "S", "R", "N", "W", "D", "L", "B"),
  loc_validity = c("VLD", "ENT"),
  loc_vec = NULL,
  collect = FALSE
)
```

## Arguments

- con:

  A `DBIConnection` object to Watina. See
  [`connect_watina`](https://inbo.github.io/watina/reference/connect_watina.md)
  to generate one.

- filterdepth_range:

  Numeric vector of length 2. Specifies the allowed range of the depth
  of the filter below soil surface, as meters (minimum and maximum
  allowed filterdepth, respectively). This condition is only applied to
  groundwater piezometers. The second vector element cannot be smaller
  than the first. Note that 'filterdepth' takes into account *half* the
  length of the filter. It is always assumed that filters are at the
  bottom of the tube. Hence
  `filterdepth = tubelength - filterlength / 2 - [tubelength part above soil surface]`.
  If filterlength is missing, it is assumed to be 0.3 m. With
  `obswells = FALSE`, a location is kept whenever one observation well
  fulfills the condition.

- filterdepth_guess:

  Logical. Only relevant for groundwater piezometers. Defaults to
  `FALSE`. For observation wells of which tubelength is known, but not
  the part of the tubelength above soil surface (height of measuring
  point), filterdepth cannot be calculated and is missing. However,
  filterdepth will never be larger than tubelength minus half the
  filterlength; hence a maximum possible (i.e. conservative) value for
  filterdepth is given by `tubelength - filterlength / 2`. With
  `filterdepth_guess = TRUE`, filterdepth is replaced by this value when
  it cannot be calculated and tubelength is available. This is done
  before applying the `filterdepth_range` condition. To mark these
  cases, a logical variable `filterdepth_guessed` is added to the
  result: `TRUE` for wells where filterdepth was replaced; `FALSE` in
  all other rows.

- filterdepth_na:

  Logical. Are observation wells with missing filterdepth value to be
  included? Defaults to `FALSE`. With `filterdepth_guess = TRUE`, this
  has only effect on the *remaining* observation wells with missing
  filterdepth value.

- obswells:

  Logical. If `TRUE`, the returned object distinguishes all observation
  wells (see *Details*) that meet the `filterdepth_range` condition (or
  have missing filterdepth, if `filterdepth_na = TRUE`). If `FALSE` (the
  default), the returned object just distinguishes locations. In the
  latter case, the variables `obswell_installdate` and
  `obswell_stopdate` are not returned.

- obswell_aggr:

  String. Defines how the attributes of multiple observation wells per
  location that fulfill the `filterdepth_range` and `filterdepth_na`
  criteria (after filterdepth adjustment if `filterdepth_guess = TRUE`),
  are aggregated into one record **per location**:

  - `"latest"`: return attributes of the most recent observation well
    that fulfills the `filterdepth_range` and `filterdepth_na` criteria;

  - `"latest_fd"`: return attributes of the most recent observation well
    that fulfills the `filterdepth_range` condition, i.e. filterdepth
    will not be missing unless *all* retained wells have missing
    filterdepth *and* `filterdepth_na = TRUE`;

  - `"latest_sso"`: return attributes of the most recent observation
    well that fulfills the `filterdepth_range` and `filterdepth_na`
    criteria *and* for which `soilsurf_ost` (soil surface level in the
    [Ostend
    height](http://crs.bkg.bund.de/crseu/crs/eu-description.php?crs_id=Y0JFX09PU1QrJTJGK1VOQ09S)
    CRS (EPSG [5710](https://epsg.io/5710)) is not missing (unless *all*
    retained wells have missing `soilsurf_ost`);

  - `"mean"`: aggregation not by selecting an individual observation
    well, but by averaging the values of the associated variables
    `soilsurf_ost`, `measuringref_ost`, `tubelength`, `filterlength`,
    `filterdepth` for the observation wells with non-missing values
    (different wells may be involved for each variable, depending on the
    distribution of missing values). With `filterdepth_guess = TRUE`,
    the extra variabele `filterdepth_guessed` is summarised as `TRUE`
    for a location if at least one of the location's observation wells
    has `filterdepth_guessed = TRUE`.

  **In all cases** the returned value of `obswell_statecode` and
  `obswell_state` corresponds to the `"latest"` approach. The
  `obswell_aggr` argument has no effect on locations with a single
  retained observation well. It is ignored if `obswells = TRUE`.

- mask:

  An optional geospatial filter of class `sf`. If provided, only
  locations that intersect with `mask` will be returned, with the value
  of `buffer` taken into account. The CRS must be Belgian Lambert 72
  (EPSG-code [31370](https://epsg.io/31370)).

- join_mask:

  Logical. Do you want to spatially join the attribute columns of `mask`
  to the resulting tibble? The spatial join is executed with
  [`st_intersects()`](https://r-spatial.github.io/sf/reference/geos_binary_pred.html)
  as the topological operator. Beware: if the same location intersects
  with more than one element of `mask` (taking into account the value of
  `buffer`), that location will occur multiple times in the result.
  `join_mask` is ignored if `mask` is not provided.

- buffer:

  Number of meters taken as a buffer to enlarge `mask` (or shrink it, if
  `buffer < 0`) if `mask` is provided.

- bbox:

  Optional geospatial fiter (rectangle). A bounding box (class `bbox`),
  or a vector of four named elements `xmin`, `xmax`, `ymin`, `ymax`
  defining the boundary coordinates of a bounding box. If provided, only
  locations within this rectangular area will be returned. The CRS must
  be Belgian Lambert 72 (EPSG-code [31370](https://epsg.io/31370)).

- area_codes:

  An optional vector with area codes. If provided, only locations within
  the areas will be returned.

- loc_type:

  Type of the location (mainly: the type of measurement device).
  Defaults to `"P"`, i.e. only groundwater piezometers are returned by
  default. Can be a vector with multiple selected values.

- loc_validity:

  Validation status of the location. Can be a vector with multiple
  selected values, which must belong to `"VLD"`, `"ENT"`, `"DEL"` or
  `"CLD"`. Defaults to `c("VLD", "ENT")`.

- loc_vec:

  An optional vector with location codes. If provided, only locations
  are returned that are present in this vector.

- collect:

  Should the data be retrieved as a local tibble? If `FALSE` (the
  default), a `tbl_lazy` object is returned (lazy query). Hence the
  result can be further built upon before retrieving data with
  [`collect()`](https://dplyr.tidyverse.org/reference/compute.html).

## Value

By default, a `tbl_lazy` object. With `collect = TRUE` or with a
specified `mask`, a local
[`tibble`](https://tibble.tidyverse.org/reference/tibble.html) is
returned.

(TO BE ADDED: Explanation on the variable names of the returned object)

## Details

(TO BE ADDED: Explanation on the different available values of loc_type
and loc_validity)

The lazy object returns a `loc_wid` variable, for further use in
*remote* queries. However, don't use it in local objects: `loc_wid` is
not to be regarded as stable. Therefore, `collect = TRUE` does not
return `loc_wid`.

The result also provides metadata at the level of the observation well,
even when `obswells = FALSE`. In the latter case, this refers to the
variables `soilsurf_ost`, `measuringref_ost`, `tubelength`,
`filterlength`, `filterdepth`. See the argument `obswell_aggr` for
options of how to aggregate this information at the location level; by
default the latest observation well is used (per location) that meets
the criteria on filterdepth. Mind that `obswells = FALSE` and
`filterdepth_na = TRUE` may lead to missing filterdepth values at
locations which do have a value for an older observation well, but not
for the most recent one.

Please note the meaning of observation well in Watina: if there are
multiple observation wells attached to one location, these belong to
*other timeframes*! So one location always coincides with exactly one
observation well at one moment in time. Multiple observation wells can
succeed one another because of physical alterations (e.g. damage of a
piezometer). Here, the term 'observation well' is used to refer to a
fixed installed device in the field (groundwater piezometer, surface
water level measurement device).

## Note

Up to and including `watina 0.3.0`, the result was sorted according to
`area_code` and `loc_code`, both for the lazy query and the collected
result. Later versions avoid sorting in case of a lazy result, because
otherwise, when using the result inside another lazy query, this led to
'ORDER BY' constructs in SQL subqueries, which must be avoided. If you
like to print the lazy object in a sorted manner, you must add
`%>% arrange(...)` yourself.

## See also

Other functions to query the data warehouse:
[`get_chem()`](https://inbo.github.io/watina/reference/get_chem.md),
[`get_xg3()`](https://inbo.github.io/watina/reference/get_xg3.md)

## Examples

``` r
if (FALSE) { # \dontrun{
watina <- connect_watina()

library(dplyr)

get_locs(watina,
  bbox = c(
    xmin = 1.4e+5,
    xmax = 1.7e+5,
    ymin = 1.6e+5,
    ymax = 1.9e+5
  )
) %>%
  arrange(area_code, loc_code)

get_locs(
  watina,
  area_codes = c("KAL", "KBR"),
  collect = TRUE
)

get_locs(
  watina,
  area_codes = c("KAL", "KBR"),
  loc_type = c("P", "S"),
  collect = TRUE
)

get_locs(
  watina,
  area_codes = "WES"
) %>%
  count()

get_locs(
  watina,
  area_codes = "WES",
  filterdepth_guess = TRUE
) %>%
  count()

get_locs(
  watina,
  area_codes = c("KAL", "KBR"),
  loc_type = c("P", "S"),
  filterdepth_na = TRUE,
  collect = TRUE
)

# Mark the different output of:
get_locs(
  watina,
  loc_vec = c("KBRP081", "KBRP090", "KBRP095", "KBRS001"),
  loc_type = c("P", "S"),
  collect = TRUE
)
# versus:
get_locs(
  watina,
  loc_vec = c("KBRP081", "KBRP090", "KBRP095", "KBRS001"),
  collect = TRUE
)

# Returning all individual observation wells:
get_locs(
  watina,
  obswells = TRUE,
  area_codes = c("KAL", "KBR"),
  loc_type = c("P", "S"),
  collect = TRUE
)

# Different examples of aggregating observation wells at location level:
get_locs(
  watina,
  area_codes = "WES",
  filterdepth_na = TRUE,
  filterdepth_guess = TRUE,
  obswell_aggr = "latest",
  collect = TRUE
) %>%
  select(loc_code, contains("ost"), contains("filterdepth")) %>%
  head(12)

get_locs(
  watina,
  area_codes = "WES",
  filterdepth_na = TRUE,
  filterdepth_guess = TRUE,
  obswell_aggr = "mean",
  collect = TRUE
) %>%
  select(loc_code, contains("ost"), contains("filterdepth")) %>%
  head(12)

# Selecting all piezometers with status VLD of the
# province "West-Vlaanderen" (current polygon taken
# from the official WFS service):
library(sf)
library(purrr)
library(httr)
mymask <-
  "https://geo.api.vlaanderen.be/VRBG/wfs" %>%
  parse_url() %>%
  list_merge(query = list(
    request = "GetFeature",
    typeName = "VRBG:Refprv",
    cql_filter = "NAAM='West-Vlaanderen'",
    srsName = "EPSG:31370",
    outputFormat = "text/xml; subtype=gml/3.1.1"
  )) %>%
  build_url() %>%
  read_sf(crs = 31370) %>%
  st_cast("GEOMETRYCOLLECTION")
get_locs(
  watina,
  loc_validity = "VLD",
  mask = mymask,
  buffer = 0
)

# Disconnect:
dbDisconnect(watina)
} # }
```

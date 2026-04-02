# Select locations based on XG3 availability and XG3 series' properties

Select locations that comply with user-specified conditions, from a
dataset as returned by
[`get_xg3`](https://inbo.github.io/watina/reference/get_xg3.md), *or*
from a list with the outputs of
[`eval_xg3_avail`](https://inbo.github.io/watina/reference/eval_xg3_avail.md)
and
[`eval_xg3_series`](https://inbo.github.io/watina/reference/eval_xg3_series.md).
Conditions can be specified for each of the summary statistics returned
by
[`eval_xg3_avail`](https://inbo.github.io/watina/reference/eval_xg3_avail.md)
and
[`eval_xg3_series`](https://inbo.github.io/watina/reference/eval_xg3_series.md).

## Usage

``` r
selectlocs_xg3(
  data,
  xg3_type = NULL,
  max_gap = NULL,
  min_dur = NULL,
  conditions,
  verbose = TRUE,
  list = FALSE
)
```

## Arguments

- data:

  Either an object returned by
  [`get_xg3`](https://inbo.github.io/watina/reference/get_xg3.md), or a
  named list of two tibbles: `"avail"` and `"ser"`. In the latter case,
  `"avail"` must be the output of
  [`eval_xg3_avail`](https://inbo.github.io/watina/reference/eval_xg3_avail.md)
  and `"ser"` must be the output of
  [`eval_xg3_series`](https://inbo.github.io/watina/reference/eval_xg3_series.md),
  whereby each function was applied to the same dataset and used the
  same setting for the `xg3_type` argument. See Details.

- xg3_type:

  Only relevant when data is an object formatted as returned by
  [`get_xg3`](https://inbo.github.io/watina/reference/get_xg3.md). In
  that case, must be a character vector of length 1, 2 or 3, which will
  default to `"L"` if not specified. Defines the types of XG3 which are
  taken from `data` for the `eval_xg3_xxx()` functions. Specifies the
  'X' in 'XG3': either `"L"`, `"H"` and/or `"V"`. Together with the
  available variables in `data`, `xg3_type` determines the meaning of
  the variable `"combined"`.

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

- conditions:

  A data frame. See the devoted section below.

- verbose:

  Logical. If `TRUE`, give feedback on dropped locations because of
  (specific) unused conditions and other 'mismatch' reasons.

- list:

  Logical. If `FALSE` (the default), the function only returns the
  end-result (a tibble with selected location codes). If `TRUE`, the
  function returns a list with the end-result plus useful intermediate
  results (see Value).

## Value

If `list = FALSE`: a tibble with one column `loc_code` that provides the
locations selected by the conditions.

If `list = TRUE`: a list of tibbles that extends the previous end-result
with intermediate results. The below elements nrs. 2 and 3 are only
given when at least one XG3 availability condition was given, nrs. 4, 5
and 6 only when at least one XG3 series condition was given, and nr. 7
is only returned when both types of condition were given. All list
elements are named:

1.  `combined_result_filtered`: the end-result, same as given by
    `list = FALSE`.

2.  `result_avail`: the test result of each computed and tested
    *availability* statistic for each location and XG3 variable:
    'condition met' (`cond_met`) is TRUE or FALSE.

3.  `combined_result_avail`: aggregation of `result_avail` **per
    location**. Specific columns: `cond_met_avail` is `TRUE` if *all*
    availability conditions for that location were `TRUE`, and is
    `FALSE` in all other cases. `pct_cond_met_avail` is the percentage
    of 'met' availability conditions per location.

4.  `result_series`: the test result of each computed and tested
    *series* statistic for each location and XG3 series: 'condition met'
    (`cond_met`) is TRUE or FALSE.

5.  `combined_result_series_xg3var`: aggregation of `result_series`
    **per location and XG3 variable**. Two consecutive aggregation steps
    are involved here:

    1.  per XG3 series: are *all* series conditions met?

    2.  per XG3 variable: is there *at least one* series where all
        series conditions are met?

    Specific columns: `all_ser_cond_met_xg3var` is the answer to
    question 2 (TRUE/FALSE). `avg_pct_cond_met_nonpassed_series` is the
    average percentage (for a location and XG3 variable) of 'met' series
    conditions in the series where *not* all conditions were met. (Note
    that the same percentage is 100 for series where all conditions are
    met, leading to `all_ser_cond_met_xg3var = TRUE` at the level of
    location and XG3 variable.)

6.  `combined_result_series`: aggregation of
    `combined_result_series_xg3var` **per location**. Specific columns:
    `cond_met_series` is `TRUE` if *all* XG3 variables (that have series
    and on which series conditions were imposed) were `TRUE` in the
    previous aggregation (`all_ser_cond_met_xg3var = TRUE`), and is
    `FALSE` in all other cases. `pct_xg3vars_passed_ser` is the
    percentage of a location's XG3 variables (that have series and on
    which series conditions were imposed) that were `TRUE` in the
    previous aggregation (`all_ser_cond_met_xg3var = TRUE`).
    `avg_pct_cond_met_in_nonpassed_series` is the average of
    `avg_pct_cond_met_nonpassed_series` from the previous aggregation
    step, over the involved XG3 variables at the location.

7.  `combined_result`: the inner join (on `loc_code`) between
    `combined_result_avail` and `combined_result_series`. Locations that
    were dropped in either evaluation because of missing information,
    are dropped here too, because the function is to return locations
    for which all conditions hold and hence could be verified.

    The last column, `all_cond_met`, requires both
    `cond_met_avail = TRUE` and `cond_met_series = TRUE` to result in
    `TRUE` for a location.

    Notes:

    - locations with `all_cond_met = FALSE` are not discarded in this
      object;

    - filtering locations with `all_cond_met = TRUE` will result in
      exactly the locations given by `combined_result_filtered`, see
      list element 1;

    - the object is only returned when both availability and series
      condition(s) were given (at least one of each family). In the
      other cases, you can directly look at `combined_result_avail` or
      `combined_result_series`, from which `combined_result_filtered` is
      derived.

## Details

`selectlocs_xg3()` separately runs
[`eval_xg3_avail()`](https://inbo.github.io/watina/reference/eval_xg3_avail.md)
and
[`eval_xg3_series()`](https://inbo.github.io/watina/reference/eval_xg3_series.md)
on the input (`data`) if the latter conforms to the output of
[`get_xg3`](https://inbo.github.io/watina/reference/get_xg3.md). See the
documentation of
[`eval_xg3_avail`](https://inbo.github.io/watina/reference/eval_xg3_avail.md)
and
[`eval_xg3_series`](https://inbo.github.io/watina/reference/eval_xg3_series.md)
to learn more about how an 'XG3 variable' and an 'XG3 series' are
defined, and about the available summary statistics. Each condition for
evaluation + selection of locations is specific to an XG3 *variable*,
which can also be the level 'combined'. Hence, the result will depend
both on the XG3 *types* (HG3, LG3 and/or VG3) for which statistics have
been computed (specified by `xg3_type`), and on the conditions,
specified by `conditions`. See the devoted section on the `conditions`
data frame.

Only locations are returned:

- which have **all** XG3 variables, implied by `xg3_type` and present in
  `conditions`, available in `data`. (In other words, all conditions
  must be testable.)

- for which **all** conditions are met;

As the conditions imposed by the `conditions` data frame are always
evaluated as a required combination of conditions ('and'), the user must
make different calls to `selectlocs_xg3()` if different sets of
conditions are to be allowed ('or').

Regarding conditions that evaluate XG3 *series*, it is taken into
account that one location can have multiple series for the same XG3
variable. When the user provides one or more conditions for the series
of a specific XG3 variable, the condition(s) are regarded as fulfilled
('condition met') when **at least one** series is present of that XG3
variable for which **all** those conditions are met.

`selectlocs_xg3()` joins the long-formatted results of
[`eval_xg3_avail`](https://inbo.github.io/watina/reference/eval_xg3_avail.md)
and
[`eval_xg3_series`](https://inbo.github.io/watina/reference/eval_xg3_series.md)
with the `conditions` data frame in order to evaluate the conditions.
Often, this join in itself already leads to dropping specific
combinations of `loc_code` and `xg3_variable`. At least the locations
that are completely dropped in this step are reported when
`verbose = TRUE`.

For larger datasets
[`eval_xg3_series()`](https://inbo.github.io/watina/reference/eval_xg3_series.md)
can take quite some time, whereas the user may want to repeatedly try
different sets of conditions until a satisfying selection of locations
is returned. However the output of both
[`eval_xg3_avail()`](https://inbo.github.io/watina/reference/eval_xg3_avail.md)
and
[`eval_xg3_series()`](https://inbo.github.io/watina/reference/eval_xg3_series.md)
will not change as long as the data and the chosen values of `max_gap`
and `min_dur` are not altered. For that reason, the user can also
prepare a list object with the respective results of
[`eval_xg3_avail()`](https://inbo.github.io/watina/reference/eval_xg3_avail.md)
and
[`eval_xg3_series()`](https://inbo.github.io/watina/reference/eval_xg3_series.md),
which must be named as `"avail"` and `"ser"`, respectively. This list
can instead be used as data-input, and in that case `xg3_type`,
`max_gap` and `min_dur` are not needed (they will be ignored).

## Specification of the conditions data frame

Conditions can be specified for each of the summary statistics returned
by
[`eval_xg3_avail`](https://inbo.github.io/watina/reference/eval_xg3_avail.md)
and
[`eval_xg3_series`](https://inbo.github.io/watina/reference/eval_xg3_series.md).
Consequently, XG3 availability conditions and XG3 series conditions can
be distinguished.

The `conditions` parameter takes a data frame that must have the
following columns:

- `xg3_variable`:

  One of:
  `"combined","lg3_lcl","lg3_ost", "vg3_lcl", "vg3_ost","hg3_lcl","hg3_ost"`.

- `statistic`:

  Name of the statistic to be evaluated.

- `criterion`:

  Numeric. Defines the value of the statistic on which the condition
  will be based.

- `direction`:

  One of: `"min","max","equal"`. Together with `criterion`, this
  completes the condition which will be evaluated with respect to the
  specific `xg3_variable`: for `direction = "min"`, the statistic must
  be the criterion value or larger; for `direction = "max"`, the
  statistic must be the criterion value or lower; for
  `direction = "equal"`, the statistic must be equal to the criterion
  value.

Each condition is one row of the data frame. The data frame should have
at least one, and may have many. Each combination of `xg3_variable` and
`statistic` must be unique. Conditions on XG3 variables, absent from
`data` or not implied by `xg3_type`, will be dropped without warning.
Hence, it is up to the user to do sensible things.

The possible statistics for XG3 availability conditions are: *nryears,
firstyear, lastyear*.

The possible statistics for XG3 series conditions are: *ser_length,
ser_nryears, ser_rel_nryears, ser_firstyear, ser_lastyear,
ser_pval_uniform, ser_mean, ser_sd, ser_se_6y, ser_rel_sd_lcl,
ser_rel_se_6y_lcl*. The last six are not defined for the XG3 variable
'combined', and the last two are only defined for variables with a local
vertical CRS.

## See also

[`eval_xg3_avail`](https://inbo.github.io/watina/reference/eval_xg3_avail.md),
[`eval_xg3_series`](https://inbo.github.io/watina/reference/eval_xg3_series.md)

Other functions to select locations:
[`selectlocs_chem()`](https://inbo.github.io/watina/reference/selectlocs_chem.md)

## Examples

``` r
if (FALSE) { # \dontrun{
watina <- connect_watina()
library(dplyr)
mylocs <- get_locs(
  watina,
  area_codes = "TOR",
  loc_type = c("P", "S")
)
mydata <-
  mylocs %>%
  get_xg3(watina, 2000)
mydata %>% arrange(loc_code, hydroyear)
# Number of locations in mydata:
mydata %>%
  distinct(loc_code) %>%
  count()
# Number of hydrological years per location and XG3 variable:
mydata %>%
  group_by(loc_code) %>%
  collect() %>%
  summarise(
    lg3_lcl = sum(!is.na(lg3_lcl)),
    hg3_lcl = sum(!is.na(hg3_lcl)),
    vg3_lcl = sum(!is.na(vg3_lcl))
  )
conditions_df <-
  tribble(
    ~xg3_variable, ~statistic, ~criterion, ~direction,
    "lg3_lcl", "ser_lastyear", 2015, "min",
    "hg3_lcl", "ser_lastyear", 2015, "min"
  )
conditions_df
result <-
  mydata %>%
  selectlocs_xg3(
    xg3_type = c("L", "H"),
    max_gap = 1,
    min_dur = 5,
    conditions = conditions_df,
    list = TRUE
  )
# or:
# mystats <- list(avail = eval_xg3_avail(mydata,
#                                        xg3_type = c("L", "H")),
#                 ser =  eval_xg3_series(mydata,
#                                        xg3_type = c("L", "H"),
#                                        max_gap = 1,
#                                        min_dur = 5))
# result <-
#   mystats %>%
#   selectlocs_xg3(conditions = conditions_df,
#                  list = TRUE)
result$combined_result_filtered
result[2:4]
# Disconnect:
dbDisconnect(watina)
} # }
```

# Select locations based on hydrochemical data properties

Select locations that comply with user-specified conditions, from a
dataset as returned by either
[`get_chem`](https://inbo.github.io/watina/reference/get_chem.md) or
[`eval_chem`](https://inbo.github.io/watina/reference/eval_chem.md).
Conditions can be specified for each of the summary statistics returned
by [`eval_chem`](https://inbo.github.io/watina/reference/eval_chem.md).

## Usage

``` r
selectlocs_chem(
  data,
  data_type = c("data", "summary"),
  chem_var = c("P-PO4", "N-NO3", "N-NO2", "N-NH4", "HCO3", "SO4", "Cl", "Na", "K", "Ca",
    "Mg", "Fe", "Mn", "Si", "Al", "CondF", "CondL", "pHF", "pHL"),
  conditions,
  verbose = TRUE,
  list = FALSE
)
```

## Arguments

- data:

  An object as returned by either
  [`get_chem`](https://inbo.github.io/watina/reference/get_chem.md) (the
  object corresponds to real 'data') or
  [`eval_chem`](https://inbo.github.io/watina/reference/eval_chem.md)
  (the object contains summary values).

- data_type:

  A string. Either `"data"` (the default) or `"summary"`, in
  correspondence with the choice made for `data`.

- chem_var:

  Only relevant when data is an object formatted as returned by
  [`get_chem`](https://inbo.github.io/watina/reference/get_chem.md). Is
  a character vector to select chemical variables for which statistics
  will be computed. To specify chemical variables, use the codes from
  the column `chem_variable` in `data`. Together with the available
  variables in `data`, `chem_var` determines the meaning of the variable
  `"combined"`.

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
with intermediate results. All list elements are named:

1.  `combined_result_filtered`: the end-result, same as given by
    `list = FALSE`.

2.  `result`: the test result of each computed and tested statistic for
    each location and chemical variable: 'condition met' (`cond_met`) is
    TRUE or FALSE.

3.  `combined_result`: aggregation of `result` **per location**.
    Specific columns: `all_cond_met` is `TRUE` if *all* conditions for
    that location were `TRUE`, and is `FALSE` in all other cases.
    `pct_cond_met` is the percentage of 'met' availability conditions
    per location.

## Details

`selectlocs_chem()` separately runs `eval_chem` on the input (`data`) if
`data_type = "data"`. See the documentation of
[`eval_chem`](https://inbo.github.io/watina/reference/eval_chem.md) to
learn more about the available summary statistics. Each condition for
evaluation + selection of locations is specific to a chemical
*variable*, which can also be the level 'combined'. Hence, the result
will depend both on the *chemical variables* for which statistics have
been computed (specified by `chem_var`), and on the conditions,
specified by `conditions`. See the devoted section on the `conditions`
data frame.

Only locations are returned:

- which have **all** chemical variables, implied by `chem_var` and
  present in `conditions`, available in `data`. (In other words, all
  conditions must be testable.)

- for which **all** conditions are met;

As the conditions imposed by the `conditions` data frame are always
evaluated as a required combination of conditions ('and'), the user must
make different calls to `selectlocs_chem()` if different sets of
conditions are to be allowed ('or').

If `data_type = "data"`, `selectlocs_chem()` calls
[`eval_chem`](https://inbo.github.io/watina/reference/eval_chem.md). Its
`type` and `uniformity_test` arguments are derived from the
user-specified `conditions` data frame.

`selectlocs_chem()` joins the long-formatted results of
[`eval_chem`](https://inbo.github.io/watina/reference/eval_chem.md) with
the `conditions` data frame in order to evaluate the conditions. Often,
this join in itself already leads to dropping specific combinations of
`loc_code` and `chem_variable`. At least the locations that are
completely dropped in this step are reported when `verbose = TRUE`.

The user may want to repeatedly try different sets of conditions until a
satisfying selection of locations is returned. However the output of
[`eval_chem`](https://inbo.github.io/watina/reference/eval_chem.md) will
not change as long as the data are not altered. For that reason, the
user can also feed the result of
[`eval_chem()`](https://inbo.github.io/watina/reference/eval_chem.md) to
the `data` argument, with `data_type = "summary"`. In that case the
argument `chem_var` is ignored.

## Specification of the conditions data frame

Conditions can be specified for each of the summary statistics returned
by [`eval_chem`](https://inbo.github.io/watina/reference/eval_chem.md).

The `conditions` parameter takes a data frame that must have the
following columns:

- `chem_variable`:

  Can be any chemical variable code, including `"combined"`.

- `statistic`:

  Name of the statistic to be evaluated.

- `criterion`:

  Numeric. Defines the value of the statistic on which the condition
  will be based.

  For condition testing on statistics of type 'date', provide the
  numeric date representation, i.e. the number of days since 1 Jan 1970
  (older dates are negative). This can be easily calculated for a given
  '`datestring`' (e.g. "18-5-2020") with:
  `as.numeric(lubridate::dmy(datestring))`.

- `direction`:

  One of: `"min","max","equal"`. Together with `criterion`, this
  completes the condition which will be evaluated with respect to the
  specific `chem_variable`: for `direction = "min"`, the statistic must
  be the criterion value or larger; for `direction = "max"`, the
  statistic must be the criterion value or lower; for
  `direction = "equal"`, the statistic must be equal to the criterion
  value.

Each condition is one row of the data frame. The data frame should have
at least one, and may have many. Each combination of `chem_variable` and
`statistic` must be unique. Conditions on chemical variables, absent
from `data` or not implied by `chem_var`, will be dropped without
warning. Hence, it is up to the user to do sensible things.

The possible statistics for conditions on chemical variables are
documented by
[`eval_chem`](https://inbo.github.io/watina/reference/eval_chem.md).

## See also

[`eval_chem`](https://inbo.github.io/watina/reference/eval_chem.md)

Other functions to select locations:
[`selectlocs_xg3()`](https://inbo.github.io/watina/reference/selectlocs_xg3.md)

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

## EXAMPLE 1
# to prepare a condition on 'firstdate', we need its numerical value:
as.numeric(lubridate::dmy("1/1/2014"))
conditions_df <-
  tribble(
    ~chem_variable, ~statistic, ~criterion, ~direction,
    "N-NO3", "nrdates", 2, "min",
    "P-PO4", "nrdates", 2, "min",
    "P-PO4", "firstdate", 16071, "max",
    "P-PO4", "timespan_years", 5, "min"
  )
conditions_df
myresult <-
  mydata %>%
  selectlocs_chem(
    data_type = "data",
    chem_var = c("N-NO3", "P-PO4"),
    conditions = conditions_df,
    list = TRUE
  )
myresult
# or:
# mystats <- eval_chem(mydata, chem_var = c("N-NO3", "P-PO4"))
# myresult <-
#   mystats %>%
#   selectlocs_chem(data_type = "summary",
#                   conditions = conditions_df,
#                   list = TRUE)
myresult$combined_result_filtered

## EXAMPLE 2
# An example based on numeric statistics:
conditions_df <-
  tribble(
    ~chem_variable, ~statistic, ~criterion, ~direction,
    "pHF", "val_mean", 5, "max",
    "CondF", "val_pct50", 100, "min"
  )
conditions_df
mydata %>%
  selectlocs_chem(
    data_type = "data",
    chem_var = c("pHF", "CondF"),
    conditions = conditions_df
  )

# Disconnect:
dbDisconnect(watina)
} # }
```

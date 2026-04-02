# Select locations based on summary statistics (generic function)

Generic (helper) function to select locations that comply with
user-specified conditions, from either a dataset (data frame or lazy
object) with the variable's values or from a data frame with summary
statistics.

## Usage

``` r
selectlocs(
  data,
  data_type = c("data", "summary"),
  eval_fun = NULL,
  eval_args = NULL,
  conditions,
  verbose,
  list
)
```

## Arguments

- data:

  Either:

  - a dataset (data frame or lazy object) with the variable's values:
    with at least a column `loc_code`, a column with the variable name,
    and a column `value`;

  - a data frame with summary statistics: with a first column `loc_code`
    and a second column with the evaluated variable names or codes. The
    column name of the second column can vary. All other columns should
    have the name of the summary statistic, and hold its value for
    `loc_code` x variable.

- data_type:

  A string. Either `"data"` (the default) or `"summary"`, in
  correspondence with the choice made for `data`.

- eval_fun:

  The evaluation function to be run, if `data` is a dataset.

- eval_args:

  The arguments of the evaluation function, as a named list, if `data`
  is a dataset.

- conditions:

  A data frame. It must have the following columns:

  `variable`

  :   Can be any variable, including `"combined"`.

  `statistic`

  :   Name of the statistic to be evaluated.

  `criterion`

  :   Numeric. Defines the value of the statistic on which the condition
      will be based.

  `direction`

  :   One of: `"min","max","equal"`. Together with `criterion`, this
      completes the condition which will be evaluated with respect to
      the specific `chem_variable`: for `direction = "min"`, the
      statistic must be the criterion value or larger; for
      `direction = "max"`, the statistic must be the criterion value or
      lower; for `direction = "equal"`, the statistic must be equal to
      the criterion value.

  Each condition is one row of the data frame. The data frame should
  have at least one, and may have many. Each combination of
  `chem_variable` and `statistic` must be unique. Conditions on chemical
  variables, absent from `data` or not implied by `chem_var`, will be
  dropped without warning. Hence, it is up to the user to do sensible
  things.

- verbose:

  Logical. If `TRUE`, give feedback on dropped locations because of
  (specific) unused conditions and other 'mismatch' reasons.

- list:

  Logical. If `FALSE` (the default), the function only returns the
  end-result (a tibble with selected location codes). If `TRUE`, the
  function returns a list with the end-result plus useful intermediate
  results.

## Value

If `list = FALSE`: a tibble with one column `loc_code` that provides the
locations selected by the conditions.

If `list = TRUE`: a list of tibbles that extends the previous end-result
with intermediate results.

## Details

The result of the evaluation function (`eval_fun`) must produce a data
frame, formatted as declared by the second bullet under the `data`
argument.

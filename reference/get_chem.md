# Get hydrochemical data from the data warehouse

Returns hydrochemical data from the *Watina* data warehouse, either as a
lazy object or as a local tibble. The values must belong to selected
locations and to a specified timeframe.

## Usage

``` r
get_chem(
  locs,
  con,
  startdate,
  enddate = paste(day(today()), month(today()), year(today())),
  conc_type = c("mass", "eq"),
  en_range = c(-0.1, 0.1),
  en_exclude_na = FALSE,
  en_fecond_threshold = 0.0023,
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

- startdate:

  First date of the timeframe, as a string. The string must use a
  formatting of the order 'day month year', i.e. a format which can be
  interpreted by
  [`dmy`](https://lubridate.tidyverse.org/reference/ymd.html).

  Examples: `"16-1-2005"`, `"16-01-2005"`, `"1-01-2005"`, `"16/1/2005"`,
  `"16/1/05"`, `"16/1/88"` (years 69 and higher are regarded as 19xy),
  `"16/1-2005"`, `"23 Oct 99"`, `"23 Okt 99"` (supposing this notation
  follows your system locale), `"16 1-!!-2005"`, ......

- enddate:

  Last date of the timeframe, as a string. The same formatting rule must
  be applied as in `startdate`. Defaults to a string representation of
  the current system date.

- conc_type:

  A string defining the type of concentration in *ionic concentration
  variables*. Either:

  - `"mass"`: mass concentration (the default);

  - `"eq"`: equivalent concentration (= normality), referring to the
    electrical charge of the dissolved ion's main natural form.

  Note that the argument has no effect on the value of
  non-ion-variables.

- en_range:

  Numeric vector of length 2. Specifies the allowed range of water
  sample electroneutrality for ion-variable measurements (see Details).
  Both vector elements must be within the range `c(-1, 1)`, with the
  second element not being smaller than the first. Note that this
  argument only affects the selection of water samples for ionic
  concentration variables, not for non-ion variables such as pH and
  electrical conductivity. Measurements of non-ion variables are always
  returned.

- en_exclude_na:

  Logical. Should ion-variable measurements of water samples with
  missing electroneutrality value be omitted? Defaults to FALSE. A
  missing electroneutrality value is the consequence of one or more
  missing values of ionic concentration variables that are needed for
  electroneutrality calculation of the water sample. Note that this
  argument has no effect on the selection of non-ion variable
  measurements, which are always returned.

- en_fecond_threshold:

  A number (with a sensible default). May be set to `NA` or `NULL` by
  the user.

  - If `en_fecond_threshold` is a number (numeric scalar), all
    measurements from water samples with an iron (meq/l) / conductivity
    (µS/cm) ratio (`Fe/CondL`) equal to or larger than
    `en_fecond_threshold` are returned, regardless of the `en_range` and
    `en_exclude_na` arguments.

  - If `en_fecond_threshold` is set to `NA` or `NULL`, the iron /
    conductivity ratio is ignored. Hence, no exceptions are made to the
    conditions imposed by `en_range` and `en_exclude_na` (except for
    measurements of non-ion variables, which are always returned).

- collect:

  Should the data be retrieved as a local tibble? If `FALSE` (the
  default), a `tbl_lazy` object is returned (lazy query). Hence the
  result can be further built upon before retrieving data with
  [`collect()`](https://dplyr.tidyverse.org/reference/compute.html).

## Value

By default, a `tbl_lazy` object. With `collect = TRUE`, a local
[`tibble`](https://tibble.tidyverse.org/reference/tibble.html) is
returned.

**Returned Fields**

- `loc_code` (chr): location code such as KESP001, ABES001, ...

- `date` (Date): sampling date

- `lab_project_id` (chr): code or identifier of the project as used by
  the laboratory

- `lab_sample_id` (chr): code or identifier of the sample as used by the
  laboratory

- `chem_variable` (chr): abbreviation for the chemical variable
  (detailed below)

- `value` (num): measurement value

- `unit` (chr): unit

- `below_loq` (logi): is the value below the limit of quantitation for
  this analysis in the laboratory?

- `loq` (num): limit of quantitation for this analysis in the laboratory

- `elneutr` (num): value of the calculated electroneutrality (not in %)

**Possible values for `chem_variable`:**

- Al: aluminium concentration

- Ca: calcium concentration

- Cl: chloride concentration

- CondF: electrical conductivity at 25°C (measured in the field)

- CondL: electrical conductivity at 25°C (measured in the laboratory)

- Fe: iron concentration

- HCO3: bicarbonate concentration

- K: potassium concentration

- Mg: magnesium concentration

- Mn: manganese concentration

- N-NH4: ammonium (expressed as ammonium-nitrogen)

- N-NO2: nitrite (expressed as nitrite-nitrogen)

- N-NO3: nitrate (expressed as nitrate-nitrogen)

- Na: sodium concentration

- P-PO4: orthophosphate concentration (expressed as
  orthophosphate-phosphorus)

- pHF: pH (measured in the field)

- pHL: pH (measured in the laboratory)

- Si: silicon concentration

- SO4: sulphate concentration

## Details

The timeframe is a selection interval between a given `startdate` and
`enddate`.

The water samples must meet a specified electroneutrality condition, set
by `en_range`.

- This condition is however ignored when the sample's iron (meq/l) /
  conductivity (µS/cm) ratio exceeds `en_fecond_threshold` (use
  `en_fecond_threshold = NA` if you don't want this to happen).

- Further, water samples are included by default if their
  electroneutrality is `NA` (this is controlled by the `en_exclude_na`
  argument).

- Finally, please note that measurements of non-ion variables are
  *always* returned!

To retrieve all data from all water samples, use `en_range = c(-1, 1)`.

**More information about the electroneutrality**

We expect groundwater samples to have no net charge, i.e. the total
positive charge from cations must equal the total negative charge from
anions. To ensure this is true (if we ignore the inevitable margin of
error in the laboratory), we calculate:

- the sum of charges from anions (AN) in the sample as *HCO3 + SO4 +
  PO4 + Cl + NO3 + NO2*

- the sum of charges from cations (CAT) in the sample as *Ca + Mg + Na +
  K + Fe + NH4*

Then we derive the electroneutrality as *(CAT - AN)/(CAT + AN)*

If significant deviation from zero occurs, there must be analytical
errors in the concentration determinations or ions at significant
concentration levels that were not included in the analysis.

The `get_chem()` function allows for a standard tolerance of +/-0.1 for
the electroneutrality. This value can be adapted using the `en_range`
argument.

## Note

Up to and including `watina 0.3.0`, the result was sorted according to
`loc_code`, `date` and `chem_variable`, both for the lazy query and the
collected result. Later versions avoid sorting in case of a lazy result,
because otherwise, when using the result inside another lazy query, this
led to 'ORDER BY' constructs in SQL subqueries, which must be avoided.
If you like to print the lazy object in a sorted manner, you must add
`%>% arrange(...)` yourself.

## See also

Other functions to query the data warehouse:
[`get_locs()`](https://inbo.github.io/watina/reference/get_locs.md),
[`get_xg3()`](https://inbo.github.io/watina/reference/get_xg3.md)

## Examples

``` r
if (FALSE) { # \dontrun{
watina <- connect_watina()
library(dplyr)
mylocs <- get_locs(watina, area_codes = "ZWA")
mylocs %>%
  get_chem(watina, "1/1/2017") %>%
  arrange(loc_code, date, chem_variable)
mylocs %>%
  get_chem(watina, "1/1/2017", collect = TRUE)
mylocs %>%
  get_chem(watina, "1/1/2017", conc_type = "eq") %>%
  arrange(loc_code, date, chem_variable)

# compare the number of returned rows:
mylocs %>%
  get_chem(watina, "1/1/2017") %>%
  count()
mylocs %>%
  get_chem(watina, "1/1/2017", en_fecond_threshold = NA) %>%
  count()
mylocs %>%
  get_chem(watina, "1/1/2017", en_exclude_na = TRUE) %>%
  count()
mylocs %>%
  get_chem(
    watina,
    "1/1/2017",
    en_exclude_na = TRUE,
    en_fecond_threshold = NA
  ) %>%
  count()
mylocs %>%
  get_chem(watina, "1/1/2017", en_range = c(-1, 1)) %>%
  count()

# joining results to mylocs:
mylocs %>%
  get_chem(watina, "1/1/2017") %>%
  left_join(
    mylocs %>%
      select(-loc_wid),
    .
  ) %>%
  collect() %>%
  arrange(loc_code, date, chem_variable)

# Disconnect:
dbDisconnect(watina)
} # }
```

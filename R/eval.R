#' Evaluate the availability of XG3 values per location
#'
#' For a dataset as returned by \code{\link{get_xg3}},
#' return for each location the first and
#' last (hydro)year and the number of (hydro)years with available XG3 values
#' of the specified type(s) (LG3, HG3 and/or VG3).
#'
#' The column \code{xg3_variable} in the resulting tibble
#' stands for the XG3 type + the vertical CRS (see \code{\link{get_xg3}}).
#' \code{xg3_variable} is restricted to the requested XG3 types (LG3, HG3
#' and/or VG3) via the \code{xg3_type} argument, but adds an extra level
#' "\code{combined}" whenever the combination of \code{data} (which may have
#' both
#' vertical CRSes) and \code{xg3_type} implies more than one requested
#' variable.
#' The "\code{combined}" level evaluates the combined presence of all selected
#' XG3 variables at each location.
#'
#' @inheritParams filter_xg3
#'
#' @return
#' A tibble with variables \code{loc_code} (see \code{\link{get_locs}}),
#' \code{xg3_variable} (character; see Details),
#' \code{nryears}, \code{firstyear} and \code{lastyear}.
#'
#' @family functions to evaluate locations
#'
#' @examples
#' \dontrun{
#' watina <- connect_watina()
#' library(dplyr)
#' mylocs <- get_locs(watina, area_codes = "KAL")
#' mydata <-
#'  mylocs %>%
#'  get_xg3(watina, 2014)
#' mydata %>% arrange(loc_code, hydroyear)
#' eval_xg3_avail(mydata,
#'                xg3_type = c("L", "V"))
    #' # Disconnect:
#' dbDisconnect(watina)
#' }
#'
#'
#' @export
#' @importFrom rlang .data
#' @importFrom assertthat
#' assert_that
#' @importFrom dplyr
#' %>%
#' mutate
#' group_by
#' summarise
#' ungroup
eval_xg3_avail <- function(data,
                           xg3_type = c("L", "H", "V")) {

    if (missing(xg3_type)) {
        xg3_type <- match.arg(xg3_type)} else {
            assert_that(all(xg3_type %in%
                                c("L", "H", "V")),
                        msg = "You specified at least one unknown xg3_type.")
        }

    xg3_avail <-
        data %>%
        qualify_xg3(xg3_type = xg3_type)

    xg3_avail <-
        xg3_avail %>%
        group_by(.data$loc_code,
                 .data$xg3_variable) %>%
        summarise(
            nryears = sum(.data$available),
            firstyear = ifelse(first(.data$nryears) > 0,
                               min(ifelse(.data$available,
                                          .data$hydroyear,
                                          NA),
                                   na.rm = TRUE),
                               NA),
            lastyear = ifelse(first(.data$nryears) > 0,
                              max(.data$hydroyear * .data$available),
                              NA)
        ) %>%
        ungroup

    return(xg3_avail)
}

















#' Filter XG3 data per location x hydroyear
#'
#' Helper function to select the specified XG3 columns per location x hydroyear.
#'
#' @param data An object returned by \code{\link{get_xg3}}.
#' @param xg3_type Character vector of length 1, 2 or 3.
#' Defines the types of XG3 which are taken from \code{data}.
#' Specifies the 'X' in 'XG3': either \code{"L"}, \code{"H"} and/or \code{"V"}.
#' Defaults to \code{"L"}.
#'
#' @return
#' A \code{tbl_lazy} object or a tibble, which is like \code{data} but with
#' non-requested XG3 variables discarded.
#'
#' @keywords internal
#' @importFrom assertthat
#' assert_that
#' @importFrom rlang .data
#' @importFrom dplyr
#' %>%
#' select
#' contains
#' arrange
#' filter
filter_xg3 <- function(data,
                        xg3_type) {

    assert_that(all(c("loc_code", "hydroyear") %in% colnames(data)) &
                    any(grepl("g3", colnames(data))),
                msg = "data does not have the necessary 'loc_code', 'hydroyear' and XG3 columns.")

    assert_that(("L" %in% xg3_type & any(grepl("lg3", colnames(data)))) |
                ("H" %in% xg3_type & any(grepl("hg3", colnames(data)))) |
                ("V" %in% xg3_type & any(grepl("vg3", colnames(data)))),
                msg = paste("xg3_type is set to",
                            dput(xg3_type),
                            "but at least one XG3 type is missing in data.")
    )

    xg3_filtered <-
        data %>%
        select(.data$loc_code,
               .data$hydroyear,
               if ("L" %in% xg3_type) contains("lg"),
               if ("H" %in% xg3_type) contains("hg"),
               if ("V" %in% xg3_type) contains("vg")
        ) %>%
        arrange(.data$loc_code,
                .data$hydroyear)

    return(xg3_filtered)
}
















#' Qualify XG3 data per location x hydroyear
#'
#' Helper function to determine
#' the availability of the specified XG3 types per location x hydroyear.
#'
#' The column \code{xg3_variable} in the resulting tibble
#' lists the requested XG3 types, for each location and hydroyear.
#' If more than one XG3 type is requested, or if \code{data} contains both
#' vertical CRSes, an extra value "\code{combined}" is listed in the
#' \code{xg3_variable} column.
#' It evaluates the combined presence of all selected XG3 variables at each
#' location and hydroyear.
#'
#' @inheritParams filter_xg3
#'
#' @return
#' A tibble, with columns \code{xg3_variable}
#' (character; see Details) and \code{available} (logical) to denote at each
#' location whether the hydroyear has the requested XG3 values available.
#'
#' @keywords internal
#' @importFrom rlang .data
#' @importFrom tidyr
#' gather
#' @importFrom dplyr
#' %>%
#' select
#' contains
#' vars
#' mutate_at
#' collect
qualify_xg3 <- function(data,
                        xg3_type) {

    xg3_qualified <-
        data %>%
        filter_xg3(xg3_type)

    if (inherits(xg3_qualified, "tbl_lazy")) {
        xg3_qualified <- collect(xg3_qualified)
    }

    if (ncol(xg3_qualified) > 3) {
        xg3_qualified <-
            xg3_qualified %>%
            mutate(combined = select(., contains("g3")) %>%
                                apply(1, function(x) all(!is.na(x)))
            )
    }

    xg3_qualified <-
        xg3_qualified %>%
        mutate_at(.vars = vars(contains("g3")),
                      .funs = (function(x) !is.na(x))) %>%
        gather(key = "xg3_variable",
               value = "available",
               -.data$loc_code,
               -.data$hydroyear)

    return(xg3_qualified)
}












#' Identify and evaluate XG3 series per location
#'
#' For a dataset as returned by \code{\link{get_xg3}},
#' determine for each location the available multi-year XG3 series
#' and calculate summary statistics for each series.
#' Note that 'years' in this context always refers to hydroyears.
#'
#' An XG3 series is a location-specific, multi-year series of
#' LG3, HG3 and/or VG3 variables
#' and is user-restricted by \code{max_gap} (maximum allowed number of empty
#' years between 'series member' years) and
#' \code{min_dur} (minimum required length of the series).
#' Further, given a dataset of XG3 values per year and location, XG3 series are
#' always constructed \emph{as long as possible} given the aforementioned
#' restrictions.
#' For one location and XG3 variable, more than one such XG3 series may be
#' available, which implies that those XG3 series are separated by more years
#' than the
#' value of \code{max_gap}.
#'
#' The function returns summary statistics for the XG3 series that are available
#' in the dataset.
#' The XG3 series of each site
#' are numbered as 'prefix_series1', 'prefix_series2' with the
#' prefix being the value of \code{xg3_variable}.
#'
#' The column \code{xg3_variable} in the resulting tibble
#' stands for the XG3 type + the vertical CRS (see \code{\link{get_xg3}})
#' to which a series belongs.
#' \code{xg3_variable} is restricted to the requested XG3 types (LG3, HG3
#' and/or VG3) via the \code{xg3_type} argument, but adds an extra level
#' "\code{combined}" whenever the combination of \code{data} (which may have
#' both
#' vertical CRSes) and \code{xg3_type} implies more than one requested
#' variable.
#' This 'combined' level defines an XG3 series as an XG3 series where each
#' 'member' year has \strong{all} selected XG3 variables available.
#'
#' @inheritParams extract_xg3_series
#'
#' @return
#' A tibble with variables:
#' \itemize{
#' \item{\code{loc_code}}: see \code{\link{get_locs}}
#' \item{\code{xg3_variable}}: character; see Details
#' \item{\code{series}}: series ID, unique within \code{loc_code}
#' \item{\code{ser_length}}: series duration (as years), i.e. from first to
#' last year
#' \item{\code{ser_nryears}}: number of years in the series for which the
#' XG3 variable is available
#' \item{\code{ser_rel_nryears}}: the fraction \code{ser_nryears / ser_length},
#' \item{\code{ser_firstyear}}: first year in the series with XG3 variable
#' \item{\code{ser_lastyear}}: last year in the series with XG3 variable
#' \item{\code{ser_pval_uniform}}: p-value of an exact, one-sample two-sided
#' Kolmogorov-Smirnov test for the discrete uniform distribution of the member
#' years within the XG3 series.
#' The smaller the p-value,
#' the less uniform the member years are spread within a series.
#' A perfectly uniform spread results in a p-value of 1.
#' Only with larger values of \code{max_gap} this p-value can get low.
#' \item{Summary statistics based on the XG3 values,
#' i.e. excluding the 'combined' series}:
#'   \itemize{
#'   \item{\code{ser_mean}}: mean XG3 value of the series, as meters.
#'   \item{\code{ser_sd}}: standard deviation of the XG3 values of the series
#'   (an estimate of the superpopulation's standard deviation).
#'   As meters.
#'   \item{\code{ser_se_6y}}: estimated standard error of the mean XG3 for a
#'   six-year period, applying finite population correction
#'   (i.e. for design-based estimation of this mean).
#'   Hence, \code{ser_se_6y} is zero when a series has no missing years.
#'   As meters.
#'   For series shorter than six years, the estimation is still regarding a
#'   six-year period, assuming the same sampling variance as in the short
#'   series.
#'   \item{\code{ser_rel_sd_lcl}}: relative standard deviation of XG3 values,
#'   i.e. \code{ser_sd / ser_mean}.
#'   \strong{This value is only calculated for \code{vert_crs = "local"}.}
#'   \item{\code{ser_rel_se_6y_lcl}}:
#'   relative standard error of the mean XG3 for a
#'   six-year period,
#'   i.e. \code{ser_se_6y / ser_mean}.
#'   \strong{This value is only calculated for \code{vert_crs = "local"}.}
#'   }
#' }
#'
#' @seealso \code{\link{extract_xg3_series}}
#'
#' @family functions to evaluate locations
#'
#' @examples
#' \dontrun{
#' watina <- connect_watina()
#' library(dplyr)
#' mylocs <- get_locs(watina, area_codes = "KAL")
#' mydata <-
#'  mylocs %>%
#'  get_xg3(watina, 1900)
#' mydata %>% arrange(loc_code, hydroyear)
#' mydata %>%
#'   eval_xg3_series(xg3_type = c("L", "V"),
#'                   max_gap = 2,
#'                   min_dur = 5)
#' # Disconnect:
#' dbDisconnect(watina)
#' }
#'
#' @export
#' @importFrom rlang .data
#' @importFrom assertthat
#' assert_that
#' @importFrom stringr
#' str_detect
#' @importFrom stats
#' sd
#' var
#' ecdf
#' @importFrom utils
#' capture.output
#' @importFrom dplyr
#' %>%
#' mutate
#' group_by
#' summarise
#' arrange
#' filter
#' select
#' inner_join
#' n
#' first
#' ungroup
#' collect
eval_xg3_series <- function(data,
                            xg3_type = c("L", "H", "V"),
                            max_gap,
                            min_dur) {

    require_pkgs("KSgeneral")

    if (missing(xg3_type)) {
        xg3_type <- match.arg(xg3_type)} else {
            assert_that(all(xg3_type %in%
                                c("L", "H", "V")),
                        msg = "You specified at least one unknown xg3_type.")
        }

    series_memberyrs <-
        data %>%
        extract_xg3_series(xg3_type = xg3_type,
                           max_gap = max_gap,
                           min_dur = min_dur)

    # summarize series properties:
    tmpf <- tempfile()
    file.create(tmpf)
    capture.output({ # to suppress the many disc_ks_test() messages
    xg3_series_props <-
        series_memberyrs %>%
        group_by(.data$loc_code,
                 .data$xg3_variable,
                 .data$series) %>%
        summarise(
            ser_length = first(.data$series_length),
            ser_nryears = n(),
            ser_rel_nryears = .data$ser_nryears / .data$ser_length,
            ser_firstyear = min(.data$hydroyear),
            ser_lastyear = max(.data$hydroyear),
            ser_pval_uniform = KSgeneral::disc_ks_test(
                .data$hydroyear,
                ecdf(seq(.data$ser_firstyear,
                         .data$ser_lastyear)),
                exact = TRUE
            ) %>%
                .$p.value
        ) %>%
        ungroup
    },
    file = tmpf)

    # calculation of standard errors

    xg3_filtered <-
        data %>%
        filter_xg3(xg3_type = xg3_type)

    if (inherits(xg3_filtered, "tbl_lazy")) {
        xg3_filtered <- collect(xg3_filtered)
    }

    xg3_series_values <-
        xg3_filtered %>%
        gather(key = "xg3_variable",
               value = "value",
               -.data$loc_code,
               -.data$hydroyear) %>%
        inner_join(
            series_memberyrs,
            by = c("loc_code", "hydroyear", "xg3_variable")
        ) %>%
        group_by(.data$loc_code,
                 .data$xg3_variable,
                 .data$series)

    xg3_series_se <-
        xg3_series_values %>%
        summarise(
            ser_mean = mean(.data$value),
            ser_sd = sd(.data$value),
            ser_se_6y = (var(.data$value) / 6 *
                              (1 - n() / first(.data$series_length))) %>%
                                sqrt,
            ser_rel_sd_lcl = ifelse(str_detect(first(.data$xg3_variable), "_lcl"),
                                .data$ser_sd / abs(.data$ser_mean),
                                NA),
            ser_rel_se_6y_lcl = ifelse(str_detect(
                                            first(.data$xg3_variable), "_lcl"),
                                        .data$ser_se_6y / abs(.data$ser_mean),
                                        NA)
        ) %>%
        ungroup

    # put it all together...
    xg3_series_props <-
        xg3_series_props %>%
        left_join(xg3_series_se,
                  by = c("loc_code",
                              "xg3_variable",
                              "series")
                  )

    return(xg3_series_props)
}

















#' Evaluate hydrochemical data per location
#'
#' For a dataset as returned by \code{\link{get_chem}},
#' return summary statistics (data availability and/or
#' numeric properties) of
#' the specified hydrochemical variables, for each location.
#'
#' For the availability statistics, an extra level
#' "\code{combined}" is added in the column \code{chem_variable} whenever the
#' arguments \code{data} and
#' \code{chem_var} imply more than one
#' chemical variable to be investigated.
#' This 'combined' level defines data availability for a water sample as the
#' availability of data for \strong{all} corresponding chemical variables.
#'
#' @param data An object returned by \code{\link{get_chem}}.
#' @param chem_var A character vector to select chemical variables for which
#' statistics will be computed.
#' To specify chemical variables, use the
#' codes from the column \code{chem_variable} in \code{data}.
#' @param type A string defining the requested type of summary statistics.
#' See section 'Value'.
#' Either:
#' \itemize{
#' \item{\code{"avail"}:} availability statistics (the default);
#' \item{\code{"num"}:} numeric summary statistics;
#' \item{\code{"both"}:} both types will be returned.
#' }
#' @param uniformity_test Should the availability statistic
#' \code{pval_uniform_totalspan} be added (see section 'Value')?
#' Defaults to \code{FALSE} as this takes much more time to calculate than
#' everything else.
#'
#' @return
#' A tibble with variables \code{loc_code} (see \code{\link{get_locs}}),
#' \code{chem_variable} (character; see Details)
#' and summary statistics (\emph{at the level
#' of \code{loc_code} x \code{chem_variable}}) depending on the state of
#' \code{type}
#' (\code{type = "both"} combines both cases):
#'
#' With \code{type = "avail"} (the default):
#' \itemize{
#' \item{\code{nryears}}: number of calendar years for which the
#' chemical variable is available (on one date at least)
#' \item{\code{nrdates}}: number of dates at which the
#' chemical variable is available
#' \item{\code{firstdate}}: earliest date at which the
#' chemical variable is available
#' \item{\code{lastdate}}: latest date at which the
#' chemical variable is available
#' \item{\code{timespan_years}}: duration as years from the first calendar year
#' (year of \code{firstdate}) to the last calendar year
#' (year of \code{lastdate}) with data available
#' \item{\code{timespan_totalspan_ratio}}: the ratio of \code{timespan_years} to
#' the 'total span', which is the duration as years from the first to the last
#' calendar year \emph{for the whole of \strong{\code{data}}}.
#' \item{\code{nryears_totalspan_ratio}}: the ratio of \code{nryears} to
#' the 'total span'
#' \item{\code{pval_uniform_totalspan}}: Only returned with
#' \code{uniformity_test = TRUE}.
#' p-value of an exact, one-sample two-sided
#' Kolmogorov-Smirnov test for the discrete uniform distribution of the calendar
#' years \emph{with data of the chemical variable available} within the series
#' of consecutive calendar years defined by the 'total span' (see above).
#' The smaller the p-value,
#' the less uniform the years are spread within the total span.
#' A perfectly uniform spread results in a p-value of 1.
#' }
#'
#' With \code{type = "num"}: summary statistics based on the chemical values,
#' i.e. excluding the 'combined' level:
#' \itemize{
#' \item{\code{val_min}}: minimum value
#' \item{\code{val_pct10}, \code{val_pct25}, \code{val_pct50},
#' \code{val_pct75}, \code{val_pct90}}: percentiles
#' \item{\code{val_max}}: maximum value
#' \item{\code{val_range}}: range
#' \item{\code{val_mean}}: mean
#' \item{\code{val_geometric_mean}}: geometric mean.
#' Only calculated when
#' all values are strictly positive.
#' \item{\code{unit}}
#' \item{\code{prop_below_loq}}: the proportion of measurements below the limit
#' of quantification (as derived from \code{below_loq == TRUE}),
#' i.e. relative to the total number of measurements
#' \emph{for which \code{below_loq} is not \code{NA}}.
#' This statistic neglects measurements with value \code{NA} for
#' \code{below_loq}!
#' If all measurements have value \code{NA} for
#' \code{below_loq}, this statistic is set to \code{NA}.
#' }
#'
#' @family functions to evaluate locations
#'
#' @examples
#' \dontrun{
#' watina <- connect_watina()
#' library(dplyr)
#' mylocs <- get_locs(watina, area_codes = "ZWA")
#' mydata <-
#'  mylocs %>%
#'  get_chem(watina, "1/1/2010")
#' mydata %>% arrange(loc_code, date, chem_variable)
#' mydata %>%
#'   pull(date) %>%
#'   lubridate::year(.) %>%
#'   (function(x) c(firstyear = min(x), lastyear = max(x)))
#' mydata %>%
#'   eval_chem(chem_var = c("P-PO4", "N-NO3", "N-NO2", "N-NH4")) %>%
#'   arrange(desc(loc_code))
#' mydata %>%
#'   eval_chem(chem_var = c("P-PO4", "N-NO3", "N-NO2", "N-NH4"),
#'             type = "both") %>%
#'   arrange(desc(loc_code)) %>%
#'   as.data.frame() %>%
#'   head(10)
#' mydata %>%
#'   eval_chem(chem_var = c("P-PO4", "N-NO3", "N-NO2", "N-NH4"),
#'             uniformity_test = TRUE) %>%
#'   arrange(desc(loc_code)) %>%
#'   select(loc_code, chem_variable, pval_uniform_totalspan)
#' # Disconnect:
#' dbDisconnect(watina)
#' }
#'
#'
#' @export
#' @importFrom rlang .data
#' @importFrom assertthat
#' assert_that
#' is.flag
#' noNA
#' @importFrom dplyr
#' %>%
#' mutate
#' mutate_at
#' group_by
#' summarise
#' ungroup
#' left_join
#' filter
#' first
#' collect
#' @importFrom tidyr
#' spread
#' gather
#' @importFrom lubridate
#' as_date
#' @importFrom stats
#' quantile
eval_chem <- function(data,
                      chem_var = c("P-PO4", "N-NO3", "N-NO2", "N-NH4",
                                   "HCO3", "SO4", "Cl",
                                   "Na", "K", "Ca", "Mg",
                                   "Fe", "Mn", "Si", "Al",
                                   "CondF", "CondL", "pHF", "pHL"),
                      type = c("avail", "num", "both"),
                      uniformity_test = FALSE) {

    require_pkgs("KSgeneral")

    type <- match.arg(type)

    assert_that(inherits(data, what = c("tbl_lazy", "data.frame")),
                msg = "data must be a lazy object or a data frame.")
    assert_that(all(c("loc_code",
                      "date",
                      "lab_sample_id",
                      "chem_variable",
                      "value",
                      "unit",
                      "below_loq") %in% colnames(data)),
                msg = "data must provide the following variables: loc_code,
date, lab_sample_id, chem_variable, value, unit, below_loq."
    )

    assert_that(is.flag(uniformity_test), assertthat::noNA(uniformity_test))

    data <-
        data %>%
        select(.data$loc_code,
               .data$date,
               .data$lab_sample_id,
               .data$chem_variable,
               .data$value,
               .data$unit,
               .data$below_loq) %>%
        filter(.data$chem_variable %in% chem_var)

    if (inherits(data, "tbl_lazy")) {
        data <-
            collect(data) %>%
            arrange(.data$loc_code,
                    .data$date,
                    .data$chem_variable)
    }

    if (!missing(chem_var)) {
        new <- chem_var[!(chem_var %in% unique(data$chem_variable))]
        if (length(new) > 0) {
            warning("You specified chemical variables not present in data: ",
                    paste(new, collapse = ", "), ".")
        }
    }

    # 1. AVAILABILITY CALCULATIONS

    if (type != "num") {

    chem_qualified <-
        data %>%
        select(-.data$below_loq,
               -.data$unit) %>%
        spread(key = .data$chem_variable,
               value = .data$value) %>%
        mutate_at(.vars = vars(4:ncol(.)),
                  .funs = (function(x) !is.na(x)))

    if (ncol(chem_qualified) > 4) {
        chem_qualified <-
            chem_qualified %>%
            mutate(combined = select(., 4:ncol(.)) %>%
                                apply(1, function(x) all(x))
        )
    }

    chem_qualified <-
        chem_qualified %>%
        gather(key = "chem_variable",
               value = "available",
               -.data$loc_code,
               -.data$date,
               -.data$lab_sample_id)

    firstyear <- chem_qualified$date %>% year %>% min
    lastyear <- chem_qualified$date %>% year %>% max
    total_span <- lastyear - firstyear + 1

    chem_avail <-
        chem_qualified %>%
        group_by(.data$loc_code,
                 .data$chem_variable) %>%
        mutate(
            year = year(.data$date),
            nrdates = sum(.data$available),
            firstdate = as_date(ifelse(.data$nrdates > 0,
                                       min(.data$date[.data$available]),
                                       NA)),
            lastdate = as_date(ifelse(.data$nrdates > 0,
                                      max(.data$date[.data$available]),
                                      NA))
        ) %>%
        group_by(.data$loc_code,
                 .data$chem_variable,
                 .data$year) %>%
        summarise(
            nrdates = first(.data$nrdates),
            firstdate = first(.data$firstdate),
            lastdate = first(.data$lastdate),
            available = any(.data$available)
        ) %>%

        (function(df, unif = uniformity_test) {

        df1 <-
            df %>%
            summarise(
                nryears = sum(.data$available),
                nrdates = first(.data$nrdates),
                firstdate = first(.data$firstdate),
                lastdate = first(.data$lastdate),
                timespan_years = year(.data$lastdate) - year(.data$firstdate) + 1,
                timespan_totalspan_ratio = .data$timespan_years / total_span,
                nryears_totalspan_ratio = .data$nryears / total_span
            ) %>%
            ungroup

        if (unif) {
            tmpf <- tempfile()
            file.create(tmpf)
            capture.output({ # to suppress the many disc_ks_test() messages
                df2 <-
                df %>%
                summarise(
                    nryears = sum(.data$available),
                    pval_uniform_totalspan = ifelse(
                        .data$nryears > 0,
                        KSgeneral::disc_ks_test(.data$year[.data$available],
                                                ecdf(seq(firstyear,
                                                         lastyear)),
                                                exact = TRUE) %>%
                            .$p.value,
                        NA
                    )
                ) %>%
                select(-.data$nryears) %>%
                ungroup
            },
            file = tmpf)
            df1 %>%
                left_join(df2,
                          by = c("loc_code",
                                 "chem_variable")) %>%
                return()
        } else {
            return(df1)
        }

            })

    }

    # 2. NUMERIC SUMMARY

    if (type != "avail") {

    chem_num <-
        data %>%
        group_by(.data$loc_code,
                 .data$chem_variable) %>%
        summarise(
            val_min = min(.data$value),
            val_pct10 = quantile(.data$value, 0.1),
            val_pct25 = quantile(.data$value, 0.25),
            val_pct50 = quantile(.data$value, 0.5),
            val_pct75 = quantile(.data$value, 0.75),
            val_pct90 = quantile(.data$value, 0.9),
            val_max = max(.data$value),
            val_range = .data$val_max - .data$val_min,
            val_mean = mean(.data$value),
            val_geometric_mean = ifelse(all(.data$value > 0),
                                        exp(mean(log(.data$value))),
                                        NA),
            unit = first(.data$unit),
            prop_below_loq = ifelse(sum(!is.na(.data$below_loq)) > 0,
                                    sum(.data$below_loq[!is.na(.data$below_loq)]) /
                                    sum(!is.na(.data$below_loq)),
                                    NA)
        ) %>%
        ungroup

    }

    # 3. COMBINE BOTH

    chem_eval <-
        switch(type,
               "avail" = chem_avail,
               "num" = chem_num,
               "both" = chem_avail %>%
                            left_join(chem_num,
                                      by = c("loc_code",
                                             "chem_variable"))
        )

    return(chem_eval)
}






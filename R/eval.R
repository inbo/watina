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
#' mydata
#' eval_xg3_avail(mydata,
#'                xg3_type = c("L", "V"))
    #' # Disconnect:
#' DBI::dbDisconnect(watina)
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
            firstyear = ifelse(.data$nryears > 0,
                               min(.data$hydroyear),
                               NA),
            lastyear = ifelse(.data$nryears > 0,
                              max(.data$hydroyear),
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
#' collect
#' contains
#' vars
#' mutate_at
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
#' available, which implies that they are separated by more years than the
#' value of \code{max_gap}.
#'
#' The function returns the available XG3 series in the dataset for each
#' site and XG3 variable, and
#' numbers them for each site as 'prefix_series1', 'prefix_series2' with the
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
#' @param max_gap A positive integer (can be zero).
#' It is part of what the user defines as 'an XG3 series':
#' the maximum allowed time gap between two consecutive
#' XG3 values, expressed as the number of years without XG3 value.
#' @param min_dur A strictly positive integer.
#' It is part of what the user defines as 'an XG3 series':
#' the minimum required duration of an XG3 series,
#' i.e. the time (expressed as years)
#' from the first to the last year of the XG3 series.
#'
#' @inheritParams filter_xg3
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
#' \item{\code{ser_pval_uniform}}: P-value of an exact, one-sample two-sided
#' Kolmogorov-Smirnov test for the discrete uniform distribution of the member
#' years withing the XG3 series.
#' The smaller the p-value, the less uniform years are spread in the series.
#' Only with larger values of \code{max_gap} this P-value can get low.
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
#' mydata
#' mydata %>%
#'   eval_xg3_series(xg3_type = c("L", "V"),
#'                   max_gap = 2,
#'                   min_dur = 5)
#' # Disconnect:
#' DBI::dbDisconnect(watina)
#' }
#'
#' @export
#' @importFrom rlang .data
#' @importFrom assertthat
#' assert_that
#' @importFrom KSgeneral
#' disc_ks_test
#' @importFrom stringr
#' str_detect
#' @importFrom stats
#' sd
#' var
#' ecdf
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
eval_xg3_series <- function(data,
                            xg3_type = c("L", "H", "V"),
                            max_gap,
                            min_dur) {

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
    sink(tempfile()) # to suppress the many disc_ks_test() messages
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
            ser_pval_uniform = disc_ks_test(.data$hydroyear,
                                            ecdf(seq(.data$ser_firstyear,
                                                     .data$ser_lastyear)),
                                            exact = TRUE) %>%
                                .$p.value
        ) %>%
        ungroup
    sink()

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












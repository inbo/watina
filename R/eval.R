#' Evaluate the availability of XG3 values per location
#'
#' For a dataset as returned by \code{\link{get_xg3}},
#' return for each location the first and
#' last (hydro)year and the number of (hydro)years with available XG3 values
#' of the specified type(s) (LG3, HG3 and/or VG3).
#'
#' The column \code{xg3_variable} in the resulting tibble
#' lists the requested XG3 types, for each location.
#' If more than one XG3 type is requested, or if \code{data} contains both
#' vertical CRSes, an extra value "\code{combined}" is listed in the
#' \code{xg3_variable} column.
#' It evaluates the combined presence of the involved XG3 variables at each
#' location.
#'
#' @inheritParams qualify_xg3
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
#' @importFrom dplyr
#' %>%
#' mutate
#' group_by
#' summarise
eval_xg3_avail <- function(data,
                           xg3_type = c("L", "H", "V")) {

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
        )

    return(xg3_avail)
}












#' Qualify XG3 data per location x hydroyear
#'
#' Helper function to select the specified XG3 columns and determine
#' their availability per location x hydroyear.
#'
#' The column \code{xg3_variable} in the resulting tibble
#' lists the requested XG3 types, for each location and hydroyear.
#' If more than one XG3 type is requested, or if \code{data} contains both
#' vertical CRSes, an extra value "\code{combined}" is listed in the
#' \code{xg3_variable} column.
#' It evaluates the combined presence of the involved XG3 variables at each
#' location and hydroyear.
#'
#' @param data An object returned by \code{\link{get_xg3}}.
#' @param xg3_type Character vector of length 1, 2 or 3.
#' Defines the types of XG3 which are taken to evaluate the hydroyears for
#' each location.
#' Specifies the 'X' in 'XG3': either \code{"L"}, \code{"H"} and/or \code{"V"}.
#'
#' @return
#' A \code{tbl_lazy} object or a tibble, with columns \code{xg3_variable}
#' (character; see Details) and \code{available} (logical) to denote at each
#' location whether the hydroyear has the requested XG3 values available.
#'
#' @keywords internal
#' @importFrom assertthat
#' assert_that
#' @importFrom rlang .data
#' @importFrom tidyr
#' gather
#' @importFrom dplyr
#' %>%
#' select
#' collect
#' contains
#' arrange
#' filter
#' vars
#' mutate_at
qualify_xg3 <- function(data,
                        xg3_type) {

    if (missing(xg3_type)) {
        xg3_type <- match.arg(xg3_type)} else {
            assert_that(all(xg3_type %in%
                                c("L", "H", "V")),
                        msg = "You specified at least one unknown xg3_type.")
        }

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

    xg3_qualified <-
        data %>%
        select(.data$loc_code,
               .data$hydroyear,
               if ("L" %in% xg3_type) contains("lg"),
               if ("H" %in% xg3_type) contains("hg"),
               if ("V" %in% xg3_type) contains("vg")
        ) %>%
        arrange(.data$loc_code,
                .data$hydroyear)

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
#' \emph{'combined'} whenever the  combination of data (which may have both
#' vertical CRSes) and \code{xg3_type} results in more than one requested
#' variable.
#' This 'combined' level defines an XG3 series as an XG3 series where each
#' 'member' year has \strong{all} individual XG3 variables available.
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
#' @inheritParams qualify_xg3
#'
#' @return
#' A tibble with variables:
#' \itemize{
#' \item{\code{loc_code}}: see \code{\link{get_locs}}
#' \item{\code{xg3_variable}}: character; see Details
#' \item{\code{series}}: series ID, unique within \code{loc_code}
#' \item{\code{ser_length}}: series duration, i.e. from first to last year
#' \item{\code{ser_nryears}}: number of years in the series for which the
#' XG3 variable is available,
#' \item{\code{ser_rel_nryears}}: the fraction \code{ser_nryears / ser_length},
#' \item{\code{ser_firstyear}}: first year in the series with XG3 variable
#' \item{\code{ser_lastyear}}: last year in the series with XG3 variable
#' }
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
#' @importFrom tidyr
#' full_seq
#' complete
#' @importFrom dplyr
#' %>%
#' mutate
#' group_by
#' summarise
#' arrange
#' filter
#' select
#' inner_join
eval_xg3_series <- function(data,
                            xg3_type = c("L", "H", "V"),
                            max_gap,
                            min_dur) {

    assert_that(max_gap %% max_gap == 0 & max_gap >= 0,
                msg = "max_gap must be a positive integer value.")
    assert_that(min_dur %% min_dur == 0 & min_dur > 0,
                msg = "min_dur must be a strictly positive integer value.")

    xg3_qualification <-
        data %>%
        qualify_xg3(xg3_type = xg3_type)

    # add missing hydroyears per location:
    xg3_completed <-
        xg3_qualification %>%
        group_by(.data$loc_code,
                 .data$xg3_variable) %>%
        complete(hydroyear = full_seq(.data$hydroyear, 1),
                 fill = list(available = FALSE)) %>%
        arrange(.data$loc_code,
                .data$xg3_variable,
                .data$hydroyear)

    # identify uninterrupted series:
    xg3_unintseries <-
        xg3_completed %>%
        mutate(unint = cumsum(c(1, diff(.data$available) != 0))) %>%
        filter(.data$available) %>%
        # consecutively number uninterrupted series:
        mutate(unint = str_c(.data$xg3_variable,
                             "_unint",
                             cumsum(c(1, diff(.data$unint) != 0))
                             )
        ) %>%
        select(-.data$available)

    # identify xg3 series:
    xg3_series <-
        xg3_unintseries %>%
        # characterize uninterrupted series:
        group_by(.data$loc_code,
                 .data$xg3_variable,
                 .data$unint) %>%
        summarise(unint_start = min(.data$hydroyear),
                  unint_end = max(.data$hydroyear)) %>%
        mutate(gap_before = .data$unint_start - lag(.data$unint_end) - 1,
               meets_gap_cond = is.na(.data$gap_before) |
                                    .data$gap_before <= max_gap,
               meets_gap_cond_lead = lead(.data$meets_gap_cond, default = TRUE),
               is_pot_series_member = .data$meets_gap_cond |
                   .data$meets_gap_cond_lead |
                   .data$unint_end - .data$unint_start + 1 >= min_dur) %>%
        # identify potential xg3 series:
        filter(.data$is_pot_series_member) %>%
        mutate(series = cumsum(!.data$meets_gap_cond) + 1) %>%
        select(-.data$meets_gap_cond_lead,
               -.data$is_pot_series_member) %>%
        # remove series that are too short:
        group_by(.data$loc_code,
                 .data$xg3_variable,
                 .data$series) %>%
        mutate(series_length = max(.data$unint_end) -
                   min(.data$unint_start) + 1) %>%
        group_by(.data$loc_code,
                 .data$xg3_variable) %>%
        filter(.data$series_length >= min_dur) %>%
        # consecutively number series:
        mutate(series = str_c(.data$xg3_variable,
                              "_series",
                              cumsum(c(1, diff(.data$series) != 0))
        )
        )

    # summarize series properties:
    xg3_series_props <-
        xg3_unintseries %>%
        inner_join(xg3_series %>%
                       select(.data$loc_code,
                              .data$xg3_variable,
                              .data$unint,
                              .data$series,
                              .data$series_length),
                   by = c("loc_code",
                          "xg3_variable",
                          "unint")) %>%
        group_by(.data$loc_code,
                 .data$xg3_variable,
                 .data$series) %>%
        summarise(
            ser_length = first(.data$series_length),
            ser_nryears = n(),
            ser_rel_nryears = .data$ser_nryears / .data$ser_length,
            ser_firstyear = min(.data$hydroyear),
            ser_lastyear = max(.data$hydroyear)
        )

    # to do: add remaining properties

    return(xg3_series_props)
}












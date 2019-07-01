#' Identify XG3 series per location
#'
#' For a dataset as returned by \code{\link{get_xg3}},
#' determine for each location the available multi-year XG3 series.
#' The function is called by \code{\link{eval_xg3_series}}.
#' Contrary to \code{\link{eval_xg3_series}}, it lists the member years of
#' each series as separate lines.
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
#' \item{\code{hydroyear}}: each member year of a series is listed separately
#' \item{\code{series}}: series ID, unique within \code{loc_code}
#' \item{\code{series_length}}: series duration, i.e. from first to last year
#' }
#'
#' @seealso \code{\link{eval_xg3_series}}
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
#'   extract_xg3_series(xg3_type = c("L", "V"),
#'                      max_gap = 2,
#'                      min_dur = 5)
#' # Disconnect:
#' DBI::dbDisconnect(watina)
#' }
#'
#' @export
#' @importFrom rlang .data
#' @importFrom assertthat
#' assert_that
#' @importFrom stringr
#' str_c
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
#' lag
#' lead
#' ungroup
extract_xg3_series <- function(data,
                               xg3_type = c("L", "H", "V"),
                               max_gap,
                               min_dur) {

    if (missing(xg3_type)) {
        xg3_type <- match.arg(xg3_type)} else {
            assert_that(all(xg3_type %in%
                                c("L", "H", "V")),
                        msg = "You specified at least one unknown xg3_type.")
        }

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

    # return member years per series:
    series_memberyrs <-
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
        ungroup %>%
        select(-.data$unint) %>%
        arrange(.data$loc_code,
                .data$xg3_variable,
                .data$hydroyear)

    return(series_memberyrs)
}












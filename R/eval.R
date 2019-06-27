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






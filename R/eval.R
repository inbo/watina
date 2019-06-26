#' Evaluate the availability of XG3 values per location
#'
#' For a dataset as returned by \code{\link{get_xg3}},
#' return for each location the first and
#' last (hydro)year and the number of (hydro)years with available XG3 values
#' of the specified type(s) (LG3, HG3 and/or VG3).
#'
#' @param data An object returned by \code{\link{get_xg3}}.
#' @param xg3_type Character vector of length 1, 2 or 3.
#' Defines the types of XG3 which are taken to evaluate the hydroyears for
#' each location: either \code{"LG3"}, \code{"HG3"} and/or \code{"VG3"}.
#' @param combined Logical.
#' If \code{xg3_type} has length two or three:
#' for a hydroyear to be evaluated as 'XG3 is available',
#' should \emph{all} selected XG3 types be non-\code{NA}
#' (\code{combined = TRUE}),
#' or \emph{at least one} (\code{combined = FALSE})?
#'
#' @return
#' A tibble with variables \code{loc_code} (see \code{\link{get_locs}}),
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
#' eval_xg3_avail(mydata,
#'                xg3_type = c("L", "V"),
#'                combined = FALSE)
#' # Disconnect:
#' DBI::dbDisconnect(watina)
#' }
#'
#'
#' @export
#' @importFrom assertthat
#' assert_that
#' @importFrom rlang .data
#' @importFrom dplyr
#' %>%
#' select
#' collect
#' contains
#' arrange
#' filter
#' mutate
#' group_by
#' summarise
eval_xg3_avail <- function(data,
                           xg3_type = c("L", "H", "V"),
                           combined = TRUE) {

    if (missing(xg3_type)) {
        xg3_type <- match.arg(xg3_type)} else {
            assert_that(all(xg3_type %in%
                                c("L", "H", "V")),
                        msg = "You specified at least one unknown xg3_type.")
        }

    xg3_avail <-
        data %>%
        select(.data$loc_code,
               .data$hydroyear,
               if ("L" %in% xg3_type) contains("lg"),
               if ("H" %in% xg3_type) contains("hg"),
               if ("V" %in% xg3_type) contains("vg")
        ) %>%
        arrange(.data$loc_code,
                .data$hydroyear)

    if (inherits(xg3_avail, "tbl_lazy")) xg3_avail <- collect(xg3_avail)

    xg3_avail <-
        xg3_avail %>%
        mutate(meets_condition = if (combined) {
            select(., contains("g3")) %>%
                apply(1, function(x) all(!is.na(x)))
        } else {
            select(., contains("g3")) %>%
                apply(1, function(x) any(!is.na(x)))
        }) %>%
        select(.data$loc_code,
               .data$hydroyear,
               .data$meets_condition) %>%
        group_by(.data$loc_code) %>%
        summarise(
            nryears = sum(.data$meets_condition),
            firstyear = ifelse(.data$nryears > 0,
                               min(.data$hydroyear),
                               NA),
            lastyear = ifelse(.data$nryears > 0,
                              max(.data$hydroyear),
                              NA)
        )

    return(xg3_avail)
}

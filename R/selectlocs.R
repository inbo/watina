#' Select locations based on XG3 availability and XG3 series' properties
#'
#' For a dataset as returned by \code{\link{get_xg3}},
#' select the locations that comply with user-specified conditions.
#' Conditions can be specified for each of the summary statistics returned
#' by \code{\link{eval_xg3_avail}} and \code{\link{eval_xg3_series}}.
#' \code{selectlocs_xg3} calls these functions by itself.
#'
#' \code{selectlocs_xg3} separately calls \code{\link{eval_xg3_avail}} and
#' \code{\link{eval_xg3_series}} on \code{data}.
#' See their documentation to learn more about how an 'XG3 variable'
#' and an 'XG3 series' are defined, and about the available summary statistics.
#' Each condition for evaluation + selection of locations (see devoted section)
#' is specific to an XG3 \emph{variable}, which can also be
#' the level 'combined'.
#' Hence, the result will depend both on the XG3 \emph{types} (HG3, LG3 and/or
#' VG3) for which statistics have been computed (specified by \code{xg3_type}),
#' and on the conditions, specified by \code{conditions}.
#' Only locations are returned:
#' \itemize{
#' \item{
#' for which \strong{all}
#' conditions are met, i.e. for the XG3 variables implied by \code{xg3_type}
#' and which occur in \code{conditions};
#' }
#' \item{
#' which have at least one such XG3 variable available in \code{data}.
#' }
#' }
#' As the conditions imposed by the \code{conditions} dataframe are always
#' evaluated as a
#' required combination of conditions ('and'), the user must make different
#' calls to \code{selectlocs_xg3}
#' if different sets of conditions are to be allowed ('or').
#'
#' Regarding conditions that evaluate XG3 \emph{series}, it is taken into
#' account that one location can have multiple series for the same XG3
#' variable.
#' When the user provides one or more conditions for the series of a specific
#' XG3 variable, the condition(s) are regarded as fulfilled ('condition met')
#' when \strong{at least one} series is present of that XG3 variable
#' for which \strong{all} those conditions are met.
#'
#' \code{selectlocs_xg3} joins the long-formatted results of
#' \code{\link{eval_xg3_avail}} and \code{\link{eval_xg3_series}}
#' with the \code{conditions} dataframe in order to evaluate the conditions.
#' Often, this (inner) join in itself already leads to dropping specific
#' combinations of \code{loc_code} and \code{xg3_variable}.
#' At least the locations that are completely dropped in this step are reported
#' when \code{verbose = TRUE}.
#'
#' @section Specification of the conditions dataframe:
#' Conditions can be specified for each of the summary statistics returned
#' by \code{\link{eval_xg3_avail}} and \code{\link{eval_xg3_series}}.
#' Consequently, XG3 availability conditions and
#' XG3 series conditions can be distinguished.
#'
#' The \code{conditions} parameter takes a dataframe that must have the
#' following columns:
#' \describe{
#' \item{\code{xg3_variable}}{One of: \code{"combined","lg3_lcl","lg3_ost",
#' "vg3_lcl",
#' "vg3_ost","hg3_lcl","hg3_ost"}.}
#' \item{\code{statistic}}{Name of the statistic to be evaluated.}
#' \item{\code{criterion}}{Numeric. Defines the value of the statistic on which
#' the
#' condition will be based.}
#' \item{\code{direction}}{One of: \code{"min","max","equal"}.
#' Together with \code{criterion}, this completes the condition which will
#' be evaluated with respect to the specific \code{xg3_variable}:
#' for \code{direction = "min"}, the statistic must be the criterion
#' value or larger; for \code{direction = "max"}, the statistic must be
#' the criterion value or lower; for \code{direction = "equal"},
#' the statistic must be equal to the criterion value.
#' }
#' }
#'
#' Each condition is one row of the dataframe.
#' The dataframe should have at least one, and may have many.
#' Each combination of \code{xg3_variable} and \code{statistic} must be
#' unique.
#' Conditions on XG3 variables, absent from \code{data} or not implied by
#' \code{xg3_type}, will be dropped without warning.
#' Hence, it is up to the user to do sensible things.
#'
#' The possible statistics for XG3 availability conditions are: \emph{nryears,
#' firstyear, lastyear}.
#'
#' The possible statistics for XG3 series conditions are: \emph{ser_length,
#' ser_nryears, ser_rel_nryears, ser_firstyear, ser_lastyear,
#' ser_pval_uniform, ser_mean, ser_sd, ser_se_6y, ser_rel_sd_lcl,
#' ser_rel_se_6y_lcl}. The last six are not defined for the XG3 variable
#' 'combined', and the last two are only defined for variables with a local
#' vertical CRS.
#'
#' @param conditions A dataframe.
#' See the devoted section below.
#' @param verbose Logical.
#' If \code{TRUE}, give feedback on dropped locations because of
#' (specific) unused conditions and other 'mismatch' reasons.
#' @param list Logical.
#' If \code{FALSE} (the default), the function only returns the end-result
#' (a tibble with selected location codes).
#' If \code{TRUE}, the function returns a list with the end-result plus useful
#' intermediate results (see Value).
#'
#' @inheritParams extract_xg3_series
#'
#' @return
#' If \code{list = FALSE}: a tibble with one column \code{loc_code} that
#' provides the locations selected by the conditions.
#'
#' If \code{list = TRUE}: a list of tibbles that extends the previous end-result
#' with intermediate results.
#' The below elements nrs. 2 and 3 are only given when at least one XG3
#' availability
#' condition was given, nrs. 4, 5 and 6 only when at least one XG3 series
#' condition was given,
#' and nr. 7 is only returned when both types of condition were given.
#' All list elements are named:
#' \enumerate{
#' \item{\code{combined_result_filtered}}:
#' the end-result, same as given by \code{list = FALSE}.
#'
#' \item{\code{result_avail}}:
#' the test result of
#' each computed and tested \emph{availability} statistic for each location and
#' XG3 variable: 'condition met' (\code{cond_met}) is TRUE or FALSE.
#'
#' \item{\code{combined_result_avail}}:
#' aggregation of \code{result_avail} \strong{per location}.
#' Specific columns:
#' \code{cond_met_avail} is \code{TRUE} if \emph{all} availability conditions
#' for that location were \code{TRUE}, and is \code{FALSE} in all other cases.
#' \code{pct_cond_met_avail} is the percentage of 'met' availability conditions
#' per location.
#'
#' \item{\code{result_series}}:
#' the test result of
#' each computed and tested \emph{series} statistic for each location and
#' XG3 series: 'condition met' (\code{cond_met}) is TRUE or FALSE.
#'
#' \item{\code{combined_result_series_xg3var}}:
#' aggregation of \code{result_series} \strong{per location and XG3 variable}.
#' Two consecutive aggregation steps are involved here:
#' \enumerate{
#' \item{per XG3 series: are \emph{all} series conditions met?}
#' \item{per XG3 variable: is there \emph{at least one} series where all series
#' conditions are met?}
#' }
#' Specific columns:
#' \code{all_ser_cond_met_xg3var} is the answer to question 2 (TRUE/FALSE).
#' \code{avg_pct_cond_met_nonpassed_series} is the average percentage
#' (for a location and XG3 variable) of 'met'
#' series conditions in the series where \emph{not} all conditions were met.
#' (Note that the same percentage is 100 for series where all conditions are
#' met, leading to \code{all_ser_cond_met_xg3var = TRUE} at the level of
#' location and XG3 variable.)
#'
#' \item{\code{combined_result_series}}:
#' aggregation of \code{combined_result_series_xg3var} \strong{per location}.
#' Specific columns:
#' \code{cond_met_series} is \code{TRUE} if \emph{all} XG3 variables
#' (that have series and on which series conditions were imposed)
#' were \code{TRUE} in the previous aggregation
#' (\code{all_ser_cond_met_xg3var = TRUE}), and is \code{FALSE} in all other
#' cases.
#' \code{pct_xg3vars_passed_ser} is the percentage of a location's XG3 variables
#' (that have series and on which series conditions were imposed) that were
#' \code{TRUE} in the previous aggregation
#' (\code{all_ser_cond_met_xg3var = TRUE}).
#' \code{avg_pct_cond_met_in_nonpassed_series} is the average of
#' \code{avg_pct_cond_met_nonpassed_series} from the previous aggregation step,
#' over the involved XG3 variables at the location.
#'
#' \item{\code{combined_result}}:
#' the inner join (on \code{loc_code}) between \code{combined_result_avail}
#' and \code{combined_result_series}.
#' Locations that were dropped in either evaluation because of missing
#' information, are dropped here too,
#' because the function is to return locations for which all conditions hold
#' and hence could be verified.
#'
#' The last column, \code{all_cond_met}, requires both
#' \code{cond_met_avail = TRUE} and \code{cond_met_series = TRUE} to result in
#' \code{TRUE} for a location.
#'
#' Notes:
#' \itemize{
#' \item{locations with \code{all_cond_met = FALSE} are not discarded in this
#' object;}
#' \item{filtering locations with \code{all_cond_met = TRUE} will result in
#' exactly
#' the locations given by \code{combined_result_filtered}, see list element 1;}
#' \item{the object is only returned when both availability and series
#' condition(s) were given (at least one of each family).
#' In the other cases, you can directly look at \code{combined_result_avail}
#' or \code{combined_result_series}, from which \code{combined_result_filtered}
#' is derived.}
#' }
#' }
#'
#' @seealso
#' \code{\link{eval_xg3_avail}}, \code{\link{eval_xg3_series}}
#'
#' @family functions to select locations
#'
#' @examples
#' \dontrun{
#' watina <- connect_watina()
#' library(dplyr)
#' mylocs <- get_locs(watina,
#'                    area_codes = "TOR",
#'                    loc_type = c("P", "S"))
#' mydata <-
#'  mylocs %>%
#'  get_xg3(watina, 2000)
#' mydata
#' # Number of locations in mydata:
#' mydata %>% distinct(loc_code) %>% count
#' # Number of hydrological years per location and XG3 variable:
#' mydata %>%
#'   group_by(loc_code) %>%
#'   collect %>%
#'   summarise(lg3_lcl = sum(!is.na(lg3_lcl)),
#'             hg3_lcl = sum(!is.na(hg3_lcl)),
#'             vg3_lcl = sum(!is.na(vg3_lcl)))
#' conditions_df <-
#'   tribble(
#'   ~xg3_variable, ~statistic, ~criterion, ~direction,
#'   "lg3_lcl", "ser_lastyear", 2015, "min",
#'   "hg3_lcl", "ser_lastyear", 2015, "min"
#'   )
#' conditions_df
#' result <-
#'   mydata %>%
#'   selectlocs_xg3(xg3_type = c("L", "H"),
#'                   max_gap = 1,
#'                   min_dur = 5,
#'                   conditions = conditions_df,
#'                   list = TRUE)
#' result$combined_result_filtered
#' result[2:4]
#' # Disconnect:
#' DBI::dbDisconnect(watina)
#' }
#'
#' @export
#' @importFrom rlang .data
#' @importFrom assertthat
#' assert_that
#' is.flag
#' @importFrom stringr
#' str_detect
#' @importFrom tidyr
#' gather
#' @importFrom dplyr
#' %>%
#' tribble
#' tibble
#' count
#' distinct
#' inner_join
#' anti_join
#' semi_join
#' pull
#' mutate
#' group_by
#' ungroup
#' summarise
#' arrange
#' filter
#' select
#' rename
#' rowwise
#' n
selectlocs_xg3 <- function(data,
                           xg3_type = c("L", "H", "V"),
                           max_gap,
                           min_dur,
                           conditions,
                           verbose = TRUE,
                           list = FALSE) {

    if (missing(xg3_type)) {
        xg3_type <- match.arg(xg3_type)} else {
            assert_that(all(xg3_type %in%
                                c("L", "H", "V")),
                        msg = "You specified at least one unknown xg3_type.")
        }

    assert_that(inherits(conditions, "data.frame"))
    assert_that(is.flag(verbose))
    assert_that(is.flag(list))

    assert_that(all(c("xg3_variable",
                  "statistic",
                  "criterion",
                  "direction") %in%
                      colnames(conditions)),
                msg = "The conditions dataframe does not have the required column names.")

    assert_that(nrow(conditions) > 0,
        msg = "You must at least provide one condition in the conditions dataframe.")

    assert_that(all(!is.na(conditions)),
                msg = "You must not leave empty cells in the conditions dataframe.")

    assert_that(all(conditions$xg3_variable %in% c("combined",
                                                 "lg3_lcl",
                                                 "lg3_ost",
                                                 "vg3_lcl",
                                                 "vg3_ost",
                                                 "hg3_lcl",
                                                 "hg3_ost")),
                msg = "You specified unknown XG3 variables in the conditions dataframe. Please fix.")

    assert_that(is.numeric(conditions$criterion),
                msg = "The 'criterion' variable of the conditions dataframe must be numeric. Please fix.")

    assert_that(all(conditions$direction %in% c("min",
                                              "max",
                                              "equal")),
                msg = "Directions in the conditions dataframe must be 'min', 'max' or 'equal'. Please fix.")

    assert_that({
        conditions %>%
            count(.data$xg3_variable, .data$statistic) %>%
            filter(.data$n > 1) %>%
            nrow(.) == 0},
        msg = "You cannot repeat the same combination of xg3_variable and statistic in the conditions dataframe.")

    unknown_statnames <-
        conditions %>%
        distinct(.data$statistic) %>%
        anti_join(tibble(statistic = c("nryears",
                                       "firstyear",
                                       "lastyear",
                                       "ser_length",
                                       "ser_nryears",
                                       "ser_rel_nryears",
                                       "ser_firstyear",
                                       "ser_lastyear",
                                       "ser_pval_uniform",
                                       "ser_mean",
                                       "ser_sd",
                                       "ser_se_6y",
                                       "ser_rel_sd_lcl",
                                       "ser_rel_se_6y_lcl")),
                  by = "statistic") %>%
        pull(.data$statistic)

    assert_that(length(unknown_statnames) == 0,
        msg = paste0(
                "The following 'statistic' names in the conditions dataframe are unknown: ",
                paste(unknown_statnames, collapse = ", "),
                ". Please fix."
                )
        )

    cond_has_avail <-
        if (any(unique(conditions$statistic) %in%
            c("nryears", "firstyear", "lastyear"))) TRUE else FALSE

    cond_has_ser <-
        if (any(str_detect(unique(conditions$statistic), "ser_"))) TRUE else FALSE

    assert_that(cond_has_avail | cond_has_ser,
                msg = "The conditions dataframe has no known 'statistic' names.")

    ## 1. Collecting statistics

    # 1.1 Availability

    xg3_avail_full <-
        data %>%
        eval_xg3_avail(xg3_type = xg3_type) %>%
        gather(key = "statistic",
               value = "value",
               -.data$loc_code,
               -.data$xg3_variable)

    if (verbose) {
        if (any(c("firstyear", "lastyear") %in% conditions$statistic)) {
            nope <-
                xg3_avail_full %>%
                filter(.data$statistic %in% c("firstyear", "lastyear"),
                       is.na(.data$value)) %>%
                semi_join(conditions,
                           by = c("xg3_variable",
                                  "statistic"))

            for (i in unique(nope$xg3_variable)) {
                loclist <-
                    nope %>%
                    filter(.data$xg3_variable == i) %>%
                    distinct(.data$loc_code) %>%
                    pull(.data$loc_code)

                message("For xg3_variable = ",
                        i,
                        ", no firstyear/lastyear testing was possible for ",
                        length(loclist),
                        " locations, ",
                        "because zero XG3 values are available: ",
                        paste(loclist, collapse = ", "),
                        "\n")
            }
        }
    }

    xg3_avail <-
        xg3_avail_full %>%
        filter(!is.na(.data$value))

    # 1.2 Series

    xg3_series <-
        data %>%
        eval_xg3_series(xg3_type = xg3_type,
                        max_gap = max_gap,
                        min_dur = min_dur) %>%
        gather(key = "statistic",
               value = "value",
               -.data$loc_code,
               -.data$xg3_variable,
               -.data$series) %>%
        # note that the rel_xxx stats are only possible for _lcl
        filter(!is.na(.data$value))

    if (verbose) {
        dropped_locs_ser1 <-
            xg3_avail_full %>%
            distinct(.data$loc_code) %>%
            anti_join(xg3_series, by = "loc_code") %>%
            arrange(.data$loc_code) %>%
            pull(.data$loc_code)

        if (length(dropped_locs_ser1) > 0) {
            message("Dropped ",
                    length(dropped_locs_ser1),
                    " locations in series evaluation because no ",
                    "XG3 series are available at those ",
                    "locations, for the requested XG3 variables: ",
                    paste(dropped_locs_ser1, collapse = ", "),
                    "\n"
            )
        }
    }


    ## 2. Calculating test results

    # 2.1 Availability

    if (cond_has_avail) {

    result_avail <-
        xg3_avail %>%
        inner_join(conditions,
                   by = c("xg3_variable",
                          "statistic")) %>%
        mutate(cond_met =
                   ifelse(.data$direction == "min", .data$value >= .data$criterion,
                          ifelse(.data$direction == "max", .data$value <= .data$criterion,
                                 .data$value == .data$criterion)))

    combined_result_avail <-
        result_avail %>%
        group_by(.data$loc_code) %>%
        summarise(cond_met_avail = all(.data$cond_met),
                  pct_cond_met_avail =
                      round(100 * sum(.data$cond_met) / n(), 0))

    if (verbose) {
        dropped_locs_avail <-
            xg3_avail_full %>%
            distinct(.data$loc_code) %>%
            anti_join(combined_result_avail, by = "loc_code") %>%
            arrange(.data$loc_code) %>%
            pull(.data$loc_code)

        if (length(dropped_locs_avail) > 0) {
            message("Dropped ",
                    length(dropped_locs_avail),
                    " locations in availability evaluation ",
                    "because none of the given ",
                    "availability conditions were available for the requested ",
                    "XG3 variables: ",
                    paste(dropped_locs_avail, collapse = ", "),
                    "\n"
            )
        }
    }

    }


    # 2.2 Series

    if (cond_has_ser) {

    result_series <-
        xg3_series %>%
        inner_join(conditions,
                   by = c("xg3_variable",
                          "statistic")) %>%
        mutate(cond_met =
                   ifelse(.data$direction == "min", .data$value >= .data$criterion,
                          ifelse(.data$direction == "max", .data$value <= .data$criterion,
                                 .data$value == .data$criterion)))

    if (verbose) {
        dropped_locs_ser2 <-
            xg3_series %>%
            distinct(.data$loc_code) %>%
            anti_join(result_series, by = "loc_code") %>%
            arrange(.data$loc_code) %>%
            pull(.data$loc_code)

        if (length(dropped_locs_ser2) > 0) {
            message("Dropped ",
                    length(dropped_locs_ser2),
                    " locations in series evaluation ",
                    "because none of the given ",
                    "series conditions were available for the requested ",
                    "XG3 variables: ",
                    paste(dropped_locs_ser2, collapse = ", "),
                    "\n"
            )
        }
    }

    combined_result_series_xg3var <-
        result_series %>%
        # series conditions must hold within series!
        group_by(.data$loc_code,
                 .data$xg3_variable,
                 .data$series) %>%
        summarise(all_ser_cond_met = all(.data$cond_met),
                  pct_cond_met =
                      round(100 * sum(.data$cond_met) / n(), 0)) %>%
        # group_by(loc_code, xg3_variable) %>%  # is already done
        summarise(all_ser_cond_met_xg3var = any(.data$all_ser_cond_met),
                  # avg_pct_cond_met_passed_series =
                  #     round(crossprod(all_ser_cond_met,
                  #                     pct_cond_met) /
                  #           sum(all_ser_cond_met), 0),
                  avg_pct_cond_met_nonpassed_series =
                      round(crossprod(!.data$all_ser_cond_met,
                                      .data$pct_cond_met) /
                                sum(!.data$all_ser_cond_met), 0)
                  )

    combined_result_series <-
        combined_result_series_xg3var %>%
        # group_by(loc_code) %>%  # is already done
        summarise(cond_met_series = all(.data$all_ser_cond_met_xg3var),
                  pct_xg3vars_passed_ser =
                      round(100 * sum(.data$all_ser_cond_met_xg3var) / n(), 0),
                  avg_pct_cond_met_in_nonpassed_series =
                      round(mean(.data$avg_pct_cond_met_nonpassed_series,
                                 na.rm = TRUE))
                  )
    }

    ## 3. Calculating combined result avail + series

    if (cond_has_avail & cond_has_ser) {
        combined_result <-
            combined_result_avail %>%
            inner_join(combined_result_series,
                       by = c("loc_code")) %>%
            rowwise %>%
            mutate(all_cond_met = all(.data$cond_met_avail,
                                      .data$cond_met_series)) %>%
            ungroup
    } else {
        if (cond_has_avail) {
            combined_result <-
                combined_result_avail %>%
                rename(all_cond_met = .data$cond_met_avail)
        } else {
            # cond_has_ser is TRUE!
            combined_result <-
                combined_result_series %>%
                rename(all_cond_met = .data$cond_met_series)
        }
    }

    combined_result_filtered <-
        combined_result %>%
        filter(.data$all_cond_met) %>%
        select(.data$loc_code)

    ## 4. Return

    if (!list) return(combined_result_filtered)

    if (list) {

        result <-
            if (cond_has_avail & cond_has_ser) {
            list(combined_result_filtered = combined_result_filtered,
                 result_avail = result_avail,
                 combined_result_avail = combined_result_avail,
                 result_series = result_series,
                 combined_result_series_xg3var = combined_result_series_xg3var,
                 combined_result_series = combined_result_series,
                 combined_result = combined_result
                 )
            } else {
                if (cond_has_avail) {
                    list(combined_result_filtered = combined_result_filtered,
                         result_avail = result_avail,
                         combined_result_avail = combined_result_avail
                         )
                } else {
                    list(combined_result_filtered = combined_result_filtered,
                         result_series = result_series,
                         combined_result_series_xg3var = combined_result_series_xg3var,
                         combined_result_series = combined_result_series
                    )
                }
            }

        return(result)
    }
}












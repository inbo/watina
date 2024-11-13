#' Select locations based on XG3 availability and XG3 series' properties
#'
#' Select locations that comply with user-specified conditions,
#' from a dataset as returned by \code{\link{get_xg3}},
#' \emph{or} from a list with the outputs of
#' \code{\link{eval_xg3_avail}} and \code{\link{eval_xg3_series}}.
#' Conditions can be specified for each of the summary statistics returned
#' by \code{\link{eval_xg3_avail}} and \code{\link{eval_xg3_series}}.
#'
#' \code{selectlocs_xg3()} separately runs \code{eval_xg3_avail()} and
#' \code{eval_xg3_series()} on the input (\code{data}) if the latter
#' conforms to the output of \code{\link{get_xg3}}.
#' See the documentation of
#' \code{\link{eval_xg3_avail}} and \code{\link{eval_xg3_series}}
#' to learn more about how an 'XG3 variable'
#' and an 'XG3 series' are defined, and about the available summary statistics.
#' Each condition for evaluation + selection of locations
#' is specific to an XG3 \emph{variable}, which can also be
#' the level 'combined'.
#' Hence, the result will depend both on the XG3 \emph{types} (HG3, LG3 and/or
#' VG3) for which statistics have been computed (specified by \code{xg3_type}),
#' and on the conditions, specified by \code{conditions}.
#' See the devoted section on the \code{conditions} data frame.
#'
#' Only locations are returned:
#' \itemize{
#' \item{
#' which have \strong{all} XG3 variables, implied by \code{xg3_type} and
#' present in \code{conditions}, available in \code{data}.
#' (In other words, all conditions must be testable.)
#' }
#' \item{
#' for which \strong{all}
#' conditions are met;
#' }
#' }
#' As the conditions imposed by the \code{conditions} data frame are always
#' evaluated as a
#' required combination of conditions ('and'), the user must make different
#' calls to \code{selectlocs_xg3()}
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
#' \code{selectlocs_xg3()} joins the long-formatted results of
#' \code{\link{eval_xg3_avail}} and \code{\link{eval_xg3_series}}
#' with the \code{conditions} data frame in order to evaluate the conditions.
#' Often, this join in itself already leads to dropping specific
#' combinations of \code{loc_code} and \code{xg3_variable}.
#' At least the locations that are completely dropped in this step are reported
#' when \code{verbose = TRUE}.
#'
#' For larger datasets \code{eval_xg3_series()} can take quite some time,
#' whereas the user may want to repeatedly try different sets of conditions
#' until a satisfying selection of locations is returned.
#' However the output of both \code{eval_xg3_avail()} and
#' \code{eval_xg3_series()} will not change as long as the data and the chosen
#' values of \code{max_gap} and \code{min_dur} are not altered.
#' For that reason, the user can also prepare a list object with the
#' respective results of \code{eval_xg3_avail()} and \code{eval_xg3_series()},
#' which must be named as \code{"avail"} and \code{"ser"}, respectively.
#' This list can instead be used as data-input, and in that case
#' \code{xg3_type}, \code{max_gap} and \code{min_dur} are not needed
#' (they will be ignored).
#'
#' @section Specification of the conditions data frame:
#' Conditions can be specified for each of the summary statistics returned
#' by \code{\link{eval_xg3_avail}} and \code{\link{eval_xg3_series}}.
#' Consequently, XG3 availability conditions and
#' XG3 series conditions can be distinguished.
#'
#' The \code{conditions} parameter takes a data frame that must have the
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
#' Each condition is one row of the data frame.
#' The data frame should have at least one, and may have many.
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
#' @param data Either an object returned by \code{\link{get_xg3}},
#' or a named list of two tibbles: \code{"avail"} and \code{"ser"}.
#' In the latter case, \code{"avail"} must be the output of
#' \code{\link{eval_xg3_avail}} and \code{"ser"} must be the output of
#' \code{\link{eval_xg3_series}}, whereby each function was applied to the same
#' dataset and used the same setting for the \code{xg3_type} argument.
#' See Details.
#' @param xg3_type Only relevant
#' when data is an object formatted as returned by
#' \code{\link{get_xg3}}.
#' In that case, must be a character vector of length 1, 2 or 3,
#' which will default to \code{"L"} if not specified.
#' Defines the types of XG3 which are taken from \code{data} for the
#' \code{eval_xg3_xxx()} functions.
#' Specifies the 'X' in 'XG3': either \code{"L"}, \code{"H"} and/or \code{"V"}.
#' Together with the available variables in \code{data},
#' \code{xg3_type} determines the meaning of the variable \code{"combined"}.
#' @param conditions A data frame.
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
#' mydata %>% arrange(loc_code, hydroyear)
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
#' # or:
#' # mystats <- list(avail = eval_xg3_avail(mydata,
#' #                                        xg3_type = c("L", "H")),
#' #                 ser =  eval_xg3_series(mydata,
#' #                                        xg3_type = c("L", "H"),
#' #                                        max_gap = 1,
#' #                                        min_dur = 5))
#' # result <-
#' #   mystats %>%
#' #   selectlocs_xg3(conditions = conditions_df,
#' #                  list = TRUE)
#' result$combined_result_filtered
#' result[2:4]
#' # Disconnect:
#' dbDisconnect(watina)
#' }
#'
#' @export
#' @importFrom rlang .data
#' @importFrom assertthat
#' assert_that
#' is.flag
#' noNA
#' @importFrom stringr
#' str_detect
#' @importFrom tidyr
#' gather
#' complete
#' nesting
#' @importFrom dplyr
#' %>%
#' tribble
#' tibble
#' count
#' distinct
#' inner_join
#' right_join
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
                           xg3_type = NULL,
                           max_gap = NULL,
                           min_dur = NULL,
                           conditions,
                           verbose = TRUE,
                           list = FALSE) {

    assert_that(inherits(data, "list") |
                    (!is.null(max_gap) &
                         !is.null(min_dur)),
                msg = "When data is not a list, you must specify max_gap and min_dur.")

    if (missing(xg3_type) & !inherits(data, "list")) {
        xg3_type <- "L"} else {
            assert_that(all(xg3_type %in%
                                c("L", "H", "V")),
                        msg = "You specified at least one unknown xg3_type.")
        }

    if (inherits(data, "list")) {
        assert_that(has_name(data, "avail"),
                    has_name(data, "ser"),
                    inherits(data$avail, "data.frame"),
                    inherits(data$ser, "data.frame"))
    }

    if (!inherits(data, "list")) {
         assert_that(all(c("loc_code", "hydroyear") %in% colnames(data)),
                     ncol(data) > 2,
                     msg = "data does not have the required format.")
    }

    assert_that(inherits(conditions, "data.frame"))
    assert_that(is.flag(verbose), assertthat::noNA(verbose))
    assert_that(is.flag(list), assertthat::noNA(list))

    assert_that(all(c("xg3_variable",
                  "statistic",
                  "criterion",
                  "direction") %in%
                      colnames(conditions)),
                msg = "The conditions data frame does not have the required column names.")

    assert_that(nrow(conditions) > 0,
        msg = "You must at least provide one condition in the conditions data frame.")

    assert_that(all(!is.na(conditions)),
                msg = "You must not leave empty cells in the conditions data frame.")

    assert_that(all(conditions$xg3_variable %in% c("combined",
                                                 "lg3_lcl",
                                                 "lg3_ost",
                                                 "vg3_lcl",
                                                 "vg3_ost",
                                                 "hg3_lcl",
                                                 "hg3_ost")),
                msg = "You specified unknown XG3 variables in the conditions data frame. Please fix.")

    assert_that(is.numeric(conditions$criterion),
                msg = "The 'criterion' variable of the conditions data frame must be numeric. Please fix.")

    assert_that(all(conditions$direction %in% c("min",
                                              "max",
                                              "equal")),
                msg = "Directions in the conditions data frame must be 'min', 'max' or 'equal'. Please fix.")

    assert_that({
        conditions %>%
            count(.data$xg3_variable, .data$statistic) %>%
            filter(.data$n > 1) %>%
            nrow(.) == 0},
        msg = "You cannot repeat the same combination of xg3_variable and statistic in the conditions data frame.")

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
                "The following 'statistic' names in the conditions data frame are unknown: ",
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
                msg = "The conditions data frame has no known 'statistic' names.")

    ## 1. Collecting statistics

    # 1.1 Availability

    xg3_avail_full <-
        (if (inherits(data, "list")) {
            data$avail
            } else {
                data %>%
                eval_xg3_avail(xg3_type = xg3_type)
            }) %>%
        gather(key = "statistic",
               value = "value",
               -.data$loc_code,
               -.data$xg3_variable)

    # 1.2 Series

    xg3_series <-
        (if (inherits(data, "list")) {
            data$ser
        } else {
            data %>%
            eval_xg3_series(xg3_type = xg3_type,
                        max_gap = max_gap,
                        min_dur = min_dur)
            }) %>%
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
        xg3_avail_full %>%
        inner_join(conditions,
                   by = c("xg3_variable",
                          "statistic")) %>%
        mutate(cond_met =
                   ifelse(.data$direction == "min", .data$value >= .data$criterion,
                          ifelse(.data$direction == "max", .data$value <= .data$criterion,
                                 .data$value == .data$criterion)),
               cond_met = ifelse(is.na(.data$cond_met), FALSE, .data$cond_met))

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
                    "because none of the summary statistics, ",
                    "needed to test the ",
                    "availability conditions, were available for the requested ",
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
        right_join(conditions %>%
                       filter(str_detect(.data$statistic, "ser_")),
                   by = c("xg3_variable",
                          "statistic")) %>%
        complete(.data$loc_code, with(.data,
                                      nesting(xg3_variable,
                                              statistic,
                                              criterion,
                                              direction))) %>%
        mutate(cond_met =
                   ifelse(.data$direction == "min", .data$value >= .data$criterion,
                          ifelse(.data$direction == "max", .data$value <= .data$criterion,
                                 .data$value == .data$criterion)),
               cond_met = ifelse(is.na(.data$cond_met), FALSE, .data$cond_met))

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
                    "because none of the summary statistics, ",
                    "needed to test the ",
                    "series conditions, were available for the requested ",
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





























#' Select locations based on hydrochemical data properties
#'
#' Select locations that comply with user-specified conditions,
#' from a dataset as returned by either \code{\link{get_chem}} or
#' \code{\link{eval_chem}}.
#' Conditions can be specified for each of the summary statistics returned
#' by \code{\link{eval_chem}}.
#'
#' \code{selectlocs_chem()} separately runs \code{eval_chem} on the input
#' (\code{data}) if \code{data_type = "data"}.
#' See the documentation of
#' \code{\link{eval_chem}}
#' to learn more about the available summary statistics.
#' Each condition for evaluation + selection of locations
#' is specific to a chemical \emph{variable}, which can also be
#' the level 'combined'.
#' Hence, the result will depend both on the \emph{chemical variables} for
#' which statistics have been computed (specified by \code{chem_var}),
#' and on the conditions, specified by \code{conditions}.
#' See the devoted section on the \code{conditions} data frame.
#'
#' Only locations are returned:
#' \itemize{
#' \item{
#' which have \strong{all} chemical variables, implied by
#' \code{chem_var} and present in \code{conditions}, available in \code{data}.
#' (In other words, all conditions must be testable.)
#' }
#' \item{
#' for which \strong{all}
#' conditions are met;
#' }
#' }
#' As the conditions imposed by the \code{conditions} data frame are always
#' evaluated as a
#' required combination of conditions ('and'), the user must make different
#' calls to \code{selectlocs_chem()}
#' if different sets of conditions are to be allowed ('or').
#'
#' If \code{data_type = "data"}, \code{selectlocs_chem()} calls
#' \code{\link{eval_chem}}.
#' Its \code{type} and \code{uniformity_test} arguments are derived from the
#' user-specified \code{conditions} data frame.
#'
#' \code{selectlocs_chem()} joins the long-formatted results of
#' \code{\link{eval_chem}}
#' with the \code{conditions} data frame in order to evaluate the conditions.
#' Often, this join in itself already leads to dropping specific
#' combinations of \code{loc_code} and \code{chem_variable}.
#' At least the locations that are completely dropped in this step are reported
#' when \code{verbose = TRUE}.
#'
#' The user may want to repeatedly try different sets of conditions
#' until a satisfying selection of locations is returned.
#' However the output of \code{\link{eval_chem}}
#' will not change as long as the data are not altered.
#' For that reason, the user can also feed the
#' result of \code{eval_chem()} to the \code{data} argument,
#' with \code{data_type = "summary"}.
#' In that case the argument \code{chem_var} is ignored.
#'
#' @section Specification of the conditions data frame:
#' Conditions can be specified for each of the summary statistics returned
#' by \code{\link{eval_chem}}.
#'
#' The \code{conditions} parameter takes a data frame that must have the
#' following columns:
#' \describe{
#' \item{\code{chem_variable}}{Can be any chemical variable code,
#' including \code{"combined"}.}
#' \item{\code{statistic}}{Name of the statistic to be evaluated.}
#' \item{\code{criterion}}{Numeric. Defines the value of the statistic on which
#' the
#' condition will be based.
#'
#' For condition testing on statistics of type 'date', provide the numeric date
#' representation, i.e. the number of days since 1 Jan 1970 (older dates are
#' negative).
#' This can be easily calculated for a given '\code{datestring}'
#' (e.g. "18-5-2020") with:
#' \code{as.numeric(lubridate::dmy(datestring))}.}
#' \item{\code{direction}}{One of: \code{"min","max","equal"}.
#' Together with \code{criterion}, this completes the condition which will
#' be evaluated with respect to the specific \code{chem_variable}:
#' for \code{direction = "min"}, the statistic must be the criterion
#' value or larger; for \code{direction = "max"}, the statistic must be
#' the criterion value or lower; for \code{direction = "equal"},
#' the statistic must be equal to the criterion value.
#' }
#' }
#'
#' Each condition is one row of the data frame.
#' The data frame should have at least one, and may have many.
#' Each combination of \code{chem_variable} and \code{statistic} must be
#' unique.
#' Conditions on chemical variables, absent from \code{data} or not implied by
#' \code{chem_var}, will be dropped without warning.
#' Hence, it is up to the user to do sensible things.
#'
#' The possible statistics for conditions on chemical variables are documented
#' by \code{\link{eval_chem}}.
#'
#' @param data An object as returned by either \code{\link{get_chem}}
#' (the object corresponds to real 'data') or
#' \code{\link{eval_chem}} (the object contains summary values).
#' @param chem_var Only relevant
#' when data is an object formatted as returned by
#' \code{\link{get_chem}}.
#' Is a character vector to select chemical variables for which
#' statistics will be computed.
#' To specify chemical variables, use the
#' codes from the column \code{chem_variable} in \code{data}.
#' Together with the available variables in \code{data},
#' \code{chem_var} determines the meaning of the variable \code{"combined"}.
#' @param conditions A data frame.
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
#' @inheritParams selectlocs
#'
#' @return
#' If \code{list = FALSE}: a tibble with one column \code{loc_code} that
#' provides the locations selected by the conditions.
#'
#' If \code{list = TRUE}: a list of tibbles that extends the previous end-result
#' with intermediate results.
#' All list elements are named:
#' \enumerate{
#' \item{\code{combined_result_filtered}}:
#' the end-result, same as given by \code{list = FALSE}.
#'
#' \item{\code{result}}:
#' the test result of
#' each computed and tested statistic for each location and
#' chemical variable: 'condition met' (\code{cond_met}) is TRUE or FALSE.
#'
#' \item{\code{combined_result}}:
#' aggregation of \code{result} \strong{per location}.
#' Specific columns:
#' \code{all_cond_met} is \code{TRUE} if \emph{all} conditions
#' for that location were \code{TRUE}, and is \code{FALSE} in all other cases.
#' \code{pct_cond_met} is the percentage of 'met' availability conditions
#' per location.
#' }
#'
#' @seealso
#' \code{\link{eval_chem}}
#'
#' @family functions to select locations
#'
#' @examples
#' \dontrun{
#' watina <- connect_watina()
#' library(dplyr)
#' mylocs <- get_locs(watina, area_codes = "ZWA")
#' mydata <-
#'     mylocs %>%
#'     get_chem(watina, "1/1/2010")
#' mydata %>% arrange(loc_code, date, chem_variable)
#' mydata %>%
#'     pull(date) %>%
#'     lubridate::year(.) %>%
#'     (function(x) c(firstyear = min(x), lastyear = max(x)))
#'
#' ## EXAMPLE 1
#' # to prepare a condition on 'firstdate', we need its numerical value:
#' as.numeric(lubridate::dmy("1/1/2014"))
#' conditions_df <-
#'     tribble(
#'         ~chem_variable, ~statistic, ~criterion, ~direction,
#'         "N-NO3", "nrdates", 2, "min",
#'         "P-PO4", "nrdates", 2, "min",
#'         "P-PO4", "firstdate", 16071, "max",
#'         "P-PO4", "timespan_years", 5, "min"
#'     )
#' conditions_df
#' myresult <-
#'     mydata %>%
#'     selectlocs_chem(data_type = "data",
#'                     chem_var = c("N-NO3", "P-PO4"),
#'                     conditions = conditions_df,
#'                     list = TRUE)
#' myresult
#' # or:
#' # mystats <- eval_chem(mydata, chem_var = c("N-NO3", "P-PO4"))
#' # myresult <-
#' #   mystats %>%
#' #   selectlocs_chem(data_type = "summary",
#' #                   conditions = conditions_df,
#' #                   list = TRUE)
#' myresult$combined_result_filtered
#'
#' ## EXAMPLE 2
#' # An example based on numeric statistics:
#' conditions_df <-
#'     tribble(
#'         ~chem_variable, ~statistic, ~criterion, ~direction,
#'         "pHF", "val_mean", 5, "max",
#'         "CondF", "val_pct50", 100, "min"
#'     )
#' conditions_df
#' mydata %>%
#'     selectlocs_chem(data_type = "data",
#'                     chem_var = c("pHF", "CondF"),
#'                     conditions = conditions_df)
#'
#' # Disconnect:
#' dbDisconnect(watina)
#' }
#'
#' @export
#' @importFrom rlang .data
#' @importFrom assertthat
#' assert_that
#' is.flag
#' noNA
#' @importFrom dplyr
#' %>%
selectlocs_chem <- function(data,
                            data_type = c("data", "summary"),
                            chem_var = c("P-PO4", "N-NO3", "N-NO2", "N-NH4",
                                         "HCO3", "SO4", "Cl",
                                         "Na", "K", "Ca", "Mg",
                                         "Fe", "Mn", "Si", "Al",
                                         "CondF", "CondL", "pHF", "pHL"),
                            conditions,
                            verbose = TRUE,
                            list = FALSE) {

    data_type <- match.arg(data_type)

    if (data_type == "data") {
        assert_that(inherits(data, c("data.frame", "tbl_lazy")),
                    all(c("loc_code", "chem_variable", "value") %in%
                            colnames(data)),
                    ncol(data) > 3,
                    msg = "data does not have the required format (with data_type = 'data')."
        )
    }

    if (data_type == "summary") {
        assert_that(inherits(data, "data.frame"),
                    all(c("loc_code", "chem_variable") %in% colnames(data)),
                    ncol(data) > 2,
                    msg = "data does not have the required format (with data_type = 'summary')."
        )
    }

    assert_that(inherits(conditions, "data.frame"))
    assert_that(is.flag(verbose), assertthat::noNA(verbose))
    assert_that(is.flag(list), assertthat::noNA(list))

    assert_that(all(c("chem_variable",
                      "statistic",
                      "criterion",
                      "direction") %in%
                        colnames(conditions)),
                msg = "The conditions data frame does not have the required column names.")

    assert_that(nrow(conditions) > 0,
                msg = "You must at least provide one condition in the conditions data frame.")

    selectlocs(data = data,
               data_type = data_type,
               eval_fun = eval_chem,
               eval_args = list(data = data,
                                chem_var = chem_var,
                                type = if (any(str_detect(conditions$statistic, "^val_")) |
                                           "prop_below_loq" %in% conditions$statistic) {
                                            if (sum(str_detect(conditions$statistic, "^val_") |
                                                    conditions$statistic == "prop_below_loq") <
                                                nrow(conditions)) "both" else "num"
                                } else "avail",
                                uniformity_test = str_detect(conditions$statistic, "pval_uniform") %>%
                                                    any),
               conditions = conditions %>%
                                rename(variable = .data$chem_variable),
               verbose = verbose,
               list = list)

}



































#' Select locations based on summary statistics (generic function)
#'
#' Generic (helper) function to select locations that comply with
#' user-specified conditions,
#' from either a dataset (data frame or lazy object) with the variable's values or
#' from a data frame with summary statistics.
#'
#' The result of the evaluation function (\code{eval_fun}) must produce a
#' data frame, formatted as declared by the second bullet under the
#' \code{data} argument.
#'
#' @param data Either:
#' \itemize{
#' \item{a dataset (data frame or lazy object) with the variable's values}:
#' with at least a column \code{loc_code}, a column with the variable name,
#' and a column \code{value};
#' \item{a data frame with summary statistics}:
#' with a first column \code{loc_code} and a second column with the evaluated
#' variable names or codes.
#' The column name of the second column can vary.
#' All other columns should have the name of the summary statistic, and hold
#' its value for \code{loc_code} x variable.
#' }
#' @param data_type A string.
#' Either \code{"data"} (the default) or \code{"summary"}, in correspondence
#' with the choice made for \code{data}.
#' @param eval_fun The evaluation function to be run, if \code{data} is a
#' dataset.
#' @param eval_args The arguments of the evaluation function, as a named list,
#' if \code{data} is a dataset.
#' @param conditions A data frame.
#' It must have the
#' following columns:
#' \describe{
#' \item{\code{variable}}{Can be any variable,
#' including \code{"combined"}.}
#' \item{\code{statistic}}{Name of the statistic to be evaluated.}
#' \item{\code{criterion}}{Numeric. Defines the value of the statistic on which
#' the
#' condition will be based.}
#' \item{\code{direction}}{One of: \code{"min","max","equal"}.
#' Together with \code{criterion}, this completes the condition which will
#' be evaluated with respect to the specific \code{chem_variable}:
#' for \code{direction = "min"}, the statistic must be the criterion
#' value or larger; for \code{direction = "max"}, the statistic must be
#' the criterion value or lower; for \code{direction = "equal"},
#' the statistic must be equal to the criterion value.
#' }
#' }
#'
#' Each condition is one row of the data frame.
#' The data frame should have at least one, and may have many.
#' Each combination of \code{chem_variable} and \code{statistic} must be
#' unique.
#' Conditions on chemical variables, absent from \code{data} or not implied by
#' \code{chem_var}, will be dropped without warning.
#' Hence, it is up to the user to do sensible things.
#' @param verbose Logical.
#' If \code{TRUE}, give feedback on dropped locations because of
#' (specific) unused conditions and other 'mismatch' reasons.
#' @param list Logical.
#' If \code{FALSE} (the default), the function only returns the end-result
#' (a tibble with selected location codes).
#' If \code{TRUE}, the function returns a list with the end-result plus useful
#' intermediate results.
#'
#' @return
#' If \code{list = FALSE}: a tibble with one column \code{loc_code} that
#' provides the locations selected by the conditions.
#'
#' If \code{list = TRUE}: a list of tibbles that extends the previous end-result
#' with intermediate results.
#'
#' @keywords internal
#'
#' @importFrom rlang .data
#' @importFrom assertthat
#' assert_that
#' is.flag
#' noNA
#' is.string
#' has_args
#' @importFrom tidyr
#' gather
#' complete
#' nesting
#' @importFrom dplyr
#' %>%
#' count
#' distinct
#' inner_join
#' right_join
#' anti_join
#' semi_join
#' pull
#' mutate
#' group_by
#' summarise
#' arrange
#' filter
#' select
#' rename
#' n
selectlocs <- function(data,
                       data_type = c("data", "summary"),
                       eval_fun = NULL,
                       eval_args = NULL,
                       conditions,
                       verbose,
                       list) {

    data_type <- match.arg(data_type)

    if (data_type == "data") {
        assert_that(inherits(data, c("data.frame", "tbl_lazy")),
                    all(c("loc_code", "value") %in% colnames(data)),
                    ncol(data) > 2,
                    msg = "data does not have the required format (with data_type = 'data')."
                    )
        assert_that(is.function(eval_fun))
        assert_that(inherits(eval_args, "list"))
        assert_that(has_args(eval_fun, names(eval_args)))
    }

    if (data_type == "summary") {
        assert_that(inherits(data, "data.frame"),
                    "loc_code" %in% colnames(data),
                    ncol(data) > 2,
                    msg = "data does not have the required format (with data_type = 'summary')."
        )
    }

    assert_that(inherits(conditions, "data.frame"))
    assert_that(is.flag(verbose), assertthat::noNA(verbose))
    assert_that(is.flag(list), assertthat::noNA(list))

    assert_that(all(c("variable",
                      "statistic",
                      "criterion",
                      "direction") %in%
                        colnames(conditions)),
                msg = "The conditions data frame does not have the required column names.")

    assert_that(nrow(conditions) > 0,
                msg = "You must at least provide one condition in the conditions data frame.")

    assert_that(all(!is.na(conditions)),
                msg = "You must not leave empty cells in the conditions data frame.")

    assert_that(is.numeric(conditions$criterion),
                msg = "The 'criterion' variable of the conditions data frame must be numeric. Please fix.")

    assert_that(all(conditions$direction %in% c("min",
                                                "max",
                                                "equal")),
                msg = "Directions in the conditions data frame must be 'min', 'max' or 'equal'. Please fix.")

    assert_that({
        conditions %>%
            count(.data$variable, .data$statistic) %>%
            filter(.data$n > 1) %>%
            nrow(.) == 0},
        msg = "You cannot repeat the same combination of variable and statistic in the conditions data frame.")

    ## 1. Collecting statistics

    suppressWarnings(
    stats_full <-
        (if (data_type == "summary") {
            data %>%
                mutate_at(.vars = 3:ncol(.),
                          .funs = as.numeric)
        } else {
            do.call(eval_fun,
                    args = eval_args) %>%
                mutate_at(.vars = 3:ncol(.),
                          .funs = as.numeric)
        }) %>%
        gather(key = "statistic",
               value = "value",
               -1, -2) %>%
        rename(variable = 2)
    )


    ## 2. Calculating test results

        result <-
            stats_full %>%
            right_join(conditions,
                       by = c("variable",
                              "statistic")) %>%
            complete(.data$loc_code, with(.data,
                                          nesting(variable,
                                                  statistic,
                                                  criterion,
                                                  direction))) %>%
            mutate(cond_met =
                       ifelse(.data$direction == "min", .data$value >= .data$criterion,
                              ifelse(.data$direction == "max", .data$value <= .data$criterion,
                                     .data$value == .data$criterion)),
                   cond_met = ifelse(is.na(.data$cond_met), FALSE, .data$cond_met)) %>%
            arrange(.data$loc_code)

        combined_result <-
            result %>%
            group_by(.data$loc_code) %>%
            summarise(all_cond_met = all(.data$cond_met),
                      pct_cond_met =
                          round(100 * sum(.data$cond_met) / n(), 0))

        if (verbose) {
            dropped_locs <-
                stats_full %>%
                distinct(.data$loc_code) %>%
                anti_join(combined_result, by = "loc_code") %>%
                arrange(.data$loc_code) %>%
                pull(.data$loc_code)

            if (length(dropped_locs) > 0) {
                message("Dropped ",
                        length(dropped_locs),
                        " locations in availability evaluation ",
                        "because none of the summary statistics, ",
                        "needed to test the ",
                        "conditions, were available for the requested ",
                        "variables: ",
                        paste(dropped_locs, collapse = ", "),
                        "\n"
                )
            }
        }


    combined_result_filtered <-
        combined_result %>%
        filter(.data$all_cond_met) %>%
        select(.data$loc_code)

    ## 3. Return

    if (!list) return(combined_result_filtered)

    if (list) {

        result <-
                list(combined_result_filtered = combined_result_filtered,
                     result = result,
                     combined_result = combined_result
                )

        return(result)
    }

}

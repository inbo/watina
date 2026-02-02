#' Compute the ionic ratio
#'
#' Uses the output of \code{\link{get_chem}} and convert it to a new tibble with
#' the ionic ratio (and the right input format to make a Van Wirdum diagram).
#'
#' Depending on the units of the Ca and Cl concentrations, the ionic ratio gets
#' calculated as follows:
#'
#' \enumerate{
#'  \item{if the Ca and Cl concentrations are in meq/l:
#'
#'  ionic ratio = [Ca2+] / ([Ca2+] + [Cl-])}
#'  \item{if the Ca and Cl concentrations are in mg/l:
#'
#'  ionic ratio = ((Ca_mg*2)/40.078) / (((Ca_mg*2)/40.078) + (Cl_mg/35.453))}
#' }
#'
#' The result is the ionic ratio \code{ir} without units (0-1).
#'
#' @param x Output of the \code{get_chem} function (hydrochemical data retrieved
#' from Watina). It can be a lazy object as well as a local object
#' \code{(get_chem(collect = TRUE))}.
#'
#' @return
#' A tibble similar to x but with an extra column \code{ir} with the ionic
#' ratio.
#'
#' @examples
#' \dontrun{
#'   watina <- connect_watina()
#'
#'   # get the chemical data
#'   mydata <-
#'   get_locs(watina, area_codes = "ZWA") %>%
#'   get_chem(watina, "1/1/2019") %>%
#'   collect()
#'
#'   # compute ionic ratio and add as new field
#'   mydata_vw <- calc_ir(mydata)
#'   # this dataset can be used to make a Van Wirdum diagram with
#'   # ggplot_vanwirdum_background()
#'
#' }
#'
#' @export
#' @importFrom assertthat
#' assert_that
#' @importFrom dplyr
#' %>%
#' filter
#' mutate
#' select
#' summarize
#' @importFrom rlang
#' .data
#' @importFrom tidyr
#' pivot_wider

calc_ir <- function(x){

    # collect the data if needed
    if ("tbl_sql" %in% class(x)) {
        x <- x %>%
            collect
    }

    x <- x %>%
        filter(.data$chem_variable %in% c("Ca", "Cl", "CondL"))

    # check units
    # the concentrations in the dataset should be all in meq or all in mg,
    # not in both units

    assert_that(x %>%
                    filter(.data$chem_variable %in% c("Ca", "Cl")) %>%
                    select("unit") %>%
                    unique() %>%
                    nrow() == 1,
                msg = "please use the same units for Ca and Cl")

    # calculate ir

    # apparently there can be several results for the same variable and sample id
    # we take the average
    # note: the measurements for Ca and Cl are always > LOQ
    x <- x %>%
        summarize(value = mean(.data$value, na.rm = TRUE),
                  .by = c("loc_code", "date", "lab_project_id",
                          "lab_sample_id", "chem_variable",
                          "unit", "below_loq", "loq", "elneutr"))

    if ("mg/l" %in% unique(x$unit)) {
        # mg
        x <- x %>%
            select(-"unit", -"below_loq", -"loq") %>%
            pivot_wider(names_from = "chem_variable", values_from = "value") %>%
            mutate(Ca_meq = (.data$Ca*2)/40.078,
                   Cl_meq = .data$Cl/35.453,
                   ir = .data$Ca_meq/(.data$Ca_meq + .data$Cl_meq))

    } else {
        # meq
        x <- x %>%
            select(-"unit", -"below_loq", -"loq") %>%
            pivot_wider(names_from = "chem_variable", values_from = "value") %>%
            mutate(ir = .data$Ca/(.data$Ca + .data$Cl))

    }

    return(x)
    # with ir without units (0-1)

}

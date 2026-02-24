#' Compute the ionic ratio
#'
#' Calculate the ionic ratio of a water sample based on the output of
#' \code{\link{get_chem}} and add it to the input data as a new column \code{ir}.
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
#' @param data Output of the \code{get_chem} function (hydrochemical data
#' retrieved from Watina) or any dataset with at least the columns
#' \code{loc_code},
#' \code{chem_variable} (with at least concentrations for Ca and Cl),
#' \code{value}, \code{unit} and \code{date}.
#' It can be a lazy object as well as a local object
#' \code{(get_chem(collect = TRUE))}.
#'
#' @return
#' A tibble similar to the input \code{data} but with an extra column \code{ir}
#' with the calculated ionic ratio.
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
#'   mydata_with_ir <- calculate_ir(mydata)
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

calculate_ir <- function(data){

    # collect the data if needed
    if (inherits(data, "tbl_sql")) {
        data <- data %>%
            collect
    }

    assert_that(all(c("loc_code", "chem_variable",
                      "value", "unit", "date") %in% colnames(data)),
                msg = "data does not have the necessary 'loc_code', 'chem_variable', 'value', 'unit' and 'date' columns.")

    assert_that((data %>% filter(.data$chem_variable == "Ca") %>% nrow() ==
                     data %>% filter(.data$chem_variable == "Cl") %>% nrow() &
                     data %>% filter(.data$chem_variable == "Cl") %>% nrow() > 0),
                msg = "there are missing observations for the concentrations of Ca and/or Cl.")

    data_ca_cl <- data %>%
        select("loc_code", "date", "chem_variable", "value", "unit") %>%
        filter(.data$chem_variable %in% c("Ca", "Cl"))
    # note: the measurements for Ca and Cl are always > LOQ

    # check units
    # the concentrations in the dataset should be all in meq or all in mg,
    # not in both units

    assert_that(data_ca_cl %>%
                    select("unit") %>%
                    unique() %>%
                    nrow() == 1,
                msg = "please use the same units for Ca and Cl.")

    # calculate ir

    # apparently there can be several results for the same loc_code, chem_variable and date
    # we take the average
    if (data_ca_cl %>%
        summarize(n = n(),
                  .by = c("loc_code", "date",
                          "chem_variable", "unit")) %>%
        filter(n > 1) %>%
        nrow > 0
        ) {
        data_ca_cl <- data_ca_cl %>%
            summarize(value = mean(.data$value, na.rm = TRUE),
                      .by = c("loc_code", "date",
                              "chem_variable", "unit"))
        warning("There are several results for the same 'loc_code', 'chem_variable' and 'date'.
Is it as expected?
The ionic ratio will be calculated based on the average of the available measurements, but you might want to check your data.")
    }

    if ("mg/l" %in% unique(data_ca_cl$unit)) {
        # mg
        data_ca_cl <- data_ca_cl %>%
            select(-"unit") %>%
            pivot_wider(names_from = "chem_variable", values_from = "value") %>%
            mutate(Ca_meq = (.data$Ca*2)/40.078,
                   Cl_meq = .data$Cl/35.453,
                   ir = .data$Ca_meq/(.data$Ca_meq + .data$Cl_meq))

    } else {
        # meq
        data_ca_cl <- data_ca_cl %>%
            select(-"unit") %>%
            pivot_wider(names_from = "chem_variable", values_from = "value") %>%
            mutate(ir = .data$Ca/(.data$Ca + .data$Cl))

    }

    data <- data %>%
        left_join(data_ca_cl %>%
                      select("loc_code", "date", "ir"),
                  by = c("loc_code", "date"))

    return(data)
    # with ir without units (0-1)

}

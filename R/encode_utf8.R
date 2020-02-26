#' Convert encoding of character and factor variables in a dataframe
#'
#' @name convertdf_enc
#' @keywords documentation
#' @importFrom inborutils convertdf_enc
#' @export
inborutils::convertdf_enc



#' A variant of dplyr's collect() which converts dataframes to UTF-8 encoding
#' if OS is Windows
#'
#' Works as a simple \code{\link[dplyr:compute]{collect()}}, on which it is
#' based.
#' However, on a Windows OS the \code{collect()} function exported
#' by this package will convert character and factor
#' variables of dataframes to \code{UTF-8} encoding.
#'
#' The functions in this package that \emph{collect} a \code{tbl_lazy} object,
#' e.g. when \code{collect = TRUE}, do so by using this function.
#' As a convenience to the user, the function is exported to allow manual
#' implementation.
#'
#' The function \code{\link[=convertdf_enc]{convertdf_enc()}} is the workhorse
#' for the encoding conversion.
#'
#' @inheritParams dplyr::collect
#'
#' @export
#' @importFrom dplyr
#' %>%
collect <- function(x) {
    result <-
        dplyr::collect(x)

    if (.Platform$OS.type == "windows") {
        result <-
            result %>%
            convertdf_enc(to = "UTF-8")
    }

    return(result)
}



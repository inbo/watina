#' Convert encoding of character and factor variables in a dataframe
#'
#' @details
#' Encoding strings: all \code{R} platforms support \code{""} (for the
#' encoding of the current
#' locale), \code{"latin1"} and \code{"UTF-8"}.
#' See \code{\link[base]{iconv}} for more information.
#'
#' @param x A dataframe or an object (such as `sf`) with the `data.frame`
#' class
#'
#' @inheritParams base::iconv
#'
#' @md
#'
#' @return
#' The original object, with character variables (and levels of
#' (character) factor variables) converted to the specified encoding.
#'
#' @export
#' @importFrom dplyr
#' %>%
#' mutate_if
#' @importFrom assertthat
#' assert_that
#' is.string
convertdf_enc <- function(x,
                          from = "",
                          to = "UTF-8",
                          sub = NA) {

    assert_that(inherits(x, "data.frame"))
    assert_that(is.string(to))

    is_chfact <- function(vec) {
        if (is.factor(vec)) {
            is.character(levels(vec))
        } else FALSE
    }

    conv_levels <- function(fact, from, to, sub) {
        levels(fact) <- iconv(levels(fact),
                              from = from,
                              to = to,
                              sub = sub)
        return(fact)
    }

    x %>%
        mutate_if(is.character,
                  iconv,
                  from = from,
                  to = to,
                  sub = sub) %>%
        mutate_if(is_chfact,
                  conv_levels,
                  from = from,
                  to = to,
                  sub = sub)

}







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



#' Convert encoding of character and factor variables in a dataframe
#'
#' @param x A dataframe.
#' @param to Encoding string to convert to.
#' All \code{R} platforms support \code{""} (for the encoding of the current
#' locale), \code{"latin1"} and \code{"UTF-8"}.
#' See \code{\link[base]{iconv}} for more information.
#'
#' @return
#' The original dataframe, with character variables (and levels of
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
                          to = "UTF-8") {

    assert_that(inherits(x, "data.frame"))
    assert_that(is.string(to))

    is_chfact <- function(vec) {
            if (is.factor(x)) {
            is.character(levels(vec))
        } else FALSE
    }

    conv_levels <- function(fact, to) {
        levels(fact) <- iconv(levels(fact),
                              to = to)
    }

    x %>%
        mutate_if(is.character,
                  iconv,
                  to = to) %>%
        mutate_if(is_chfact,
                  conv_levels,
                  to = to)

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



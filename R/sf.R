#' Convert a dataframe with X and Y coordinates to a geospatial points object
#'
#' \code{as_points} is a convenience function which accepts as input a dataframe
#' with X/Y coordinates (in meters), assumed to come from the coordinate
#' reference system (CRS)
#' 'Belge 1972 / Belgian Lambert 72' (EPSG \href{https://epsg.io/31370}{31370}).
#' It converts the dataframe into an \code{sf} points object in the same CRS.
#'
#' As locations in Watina are typically defined by their X/Y coordinates,
#' this function eases the conversion to spatial data.
#' To later remove all spatial information from the result, you can use
#' \code{\link[sf:st_geometry]{sf::st_drop_geometry()}}.
#'
#' @param df A dataframe with X and Y coordinates in meters, assumed to be in
#' the Belgian Lambert 72 CRS (EPSG-code 31370).
#' @param xvar String. The X coordinate variable name. Defaults to \code{"x"}.
#' @param yvar String. The Y coordinate variable name. Defaults to \code{"y"}.
#' @param remove Logical. Should the X and Y coordinates be removed from the
#' dataframe after conversion to a spatial object?
#' @param warn_dupl Logical.
#' Defaults to \code{TRUE}.
#' Should the user be warned when duplicated coordinates are present in the
#' result?
#'
#' @return
#' An \code{sf} object of geometry type \code{POINT} with EPSG-code
#' 31370 as coordinate reference system.
#'
#' @examples
#' library(tibble)
#' mydata <-
#'   tibble(
#'     a = runif(5),
#'     x = rnorm(5, 155763, 5),
#'     y = rnorm(5, 132693, 5)
#'   )
#' as_points(mydata)
#'
#' @export
#' @importFrom sf st_as_sf
#' @importFrom assertthat
#' assert_that
#' is.string
#' has_name
#' @importFrom dplyr
#' %>%
as_points <- function(df,
                      xvar = "x",
                      yvar = "y",
                      remove = FALSE,
                      warn_dupl = TRUE) {

    assert_that(inherits(df, "data.frame"))
    assert_that(is.string(xvar))
    assert_that(is.string(yvar))
    assert_that(has_name(df, xvar))
    assert_that(has_name(df, yvar))
    assert_that(is.flag(warn_dupl), noNA(warn_dupl))

    df_cleaned <-
        df[!is.na(df[,xvar]) & !is.na(df[,yvar]),]

    if (nrow(df_cleaned) < nrow(df)) {
        warning(nrow(df) - nrow(df_cleaned),
                " locations were removed because of missing X or Y coordinates.")
    }

    if (warn_dupl) {
        df_cleaned %>%
            {warn_xy_duplicates(get(xvar, .), get(yvar, .))}
    }

    df_cleaned %>%
        st_as_sf(coords = c(xvar, yvar),
                 crs = 31370,
                 remove = remove)

}









#' Warn for duplicated XY coordinate pairs
#'
#' @details
#' Note that both vectors must be of same length.
#'
#' @param x Numerical vector of x coordinates
#' @param y Numerical vector of y coordinates
#'
#' @importFrom assertthat
#' assert_that
#' @importFrom dplyr
#' %>%
#' count
#' filter
#' @keywords internal
warn_xy_duplicates <- function(x, y) {

    assert_that(is.numeric(x))
    assert_that(is.numeric(y))
    assert_that(all.equal(length(x), length(y)))

    n_duplicated <-
        data.frame(x = x, y = y) %>%
        count(x, y) %>%
        filter(.data$n > 1) %>%
        nrow

    if (n_duplicated > 0) {
        if (n_duplicated == 1) {
            warning("1 coordinate pair occurs more than once.")
        } else {
            warning(n_duplicated,
                    " different coordinate pairs occur more than once.\n")
        }
    }


}

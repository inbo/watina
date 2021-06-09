#' Detect (spatial) groundwater well clusters
#'
#' \code{cluster_locs()} accepts as input a
#' dataframe with X/Y coordinates, or an \code{sf} object
#' of geometry type \code{POINT}.
#' The function adds an integer variable that defines cluster membership.
#' The intention is to detect spatial groundwater well clusters; hence it uses a
#' sensible method of spatial clustering and default euclidean distance
#' to cut the cluster tree.
#'
#' The function performs agglomerative clustering with the
#' \emph{complete linkage} method.
#' This way, the application of a tree cutoff (\code{max_dist}) means that each
#' cluster is a collection of locations with a maximum distance - between any
#' two locations of the cluster - not larger than the cutoff value.
#' All locations that can be clustered under this condition, will be.
#' Locations that can not be clustered receive a unique cluster value.
#'
#' The function's code was partly inspired by unpublished code from Ivy Jansen.
#'
#' @param input A dataframe with X/Y coordinates, or an \code{sf} object of
#' geometry type \code{POINT}.
#' A typical input dataframe is the collected output of \code{\link{get_locs}}.
#' @param max_dist The maximum geospatial distance between two points to make
#' them belong to the same cluster.
#' The default value is sensible for many usecases,
#' supposing \emph{meter} is the unit of the
#' coordinate reference system, as is the case for the
#' 'Belge 1972 / Belgian Lambert 72' CRS
#' (EPSG \href{https://epsg.io/31370}{31370}).
#' @param output_var Name of the new variable to be added to
#' \code{input}.
#' @param xvar String.
#' The X coordinate variable name; only considered when \code{input} is a
#' dataframe.
#' Defaults to \code{"x"}.
#' @param yvar String.
#' The Y coordinate variable name; only considered when \code{input} is a
#' dataframe.
#' Defaults to \code{"y"}.
#'
#' @return
#' The original object with an extra variable added (by default:
#' \code{cluster_id}) to define
#' cluster membership.
#'
#' @examples
#' library(dplyr)
#' set.seed(123456789)
#' mydata <-
#'   tibble(
#'     a = runif(10),
#'     x = rnorm(10, 155763, 2),
#'     y = rnorm(10, 132693, 2)
#'   )
#' cluster_locs(mydata) %>%
#'   arrange(cluster_id)
#' mydata %>%
#'   as_points(remove = TRUE) %>%
#'   cluster_locs %>%
#'   arrange(cluster_id)
#'
#' \dontrun{
#' watina <- connect_watina()
#'
#' clusters <-
#'   get_locs(watina,
#'            area_codes = "KBR",
#'            collect = TRUE) %>%
#'   cluster_locs
#'
#' # inspect result:
#' clusters %>%
#'   select(loc_code, x, y, cluster_id) %>%
#'   arrange(cluster_id)
#'
#' # frequency of cluster sizes:
#' clusters %>%
#'   count(cluster_id) %>%
#'   pull(n) %>%
#'   table
#'
#' # Disconnect:
#' dbDisconnect(watina)
#' }
#'
#' @export
#' @importFrom assertthat
#' assert_that
#' is.string
#' has_name
#' @importFrom dplyr
#' %>%
#' as_tibble
#' bind_cols
#' @importFrom stats
#' dist
#' hclust
#' cutree
cluster_locs <- function(input,
                          max_dist = 2,
                          output_var = "cluster_id",
                          xvar = "x",
                          yvar = "y") {

    assert_that(inherits(input, c("data.frame", "sf")))
    assert_that(is.string(xvar), is.string(yvar), is.string(output_var))

    require_pkgs("tibble")

    if (inherits(input, "sf")) {

        require_pkgs("sf")

        assert_that(unique(sf::st_geometry_type(input)) == "POINT",
                    msg = "Geospatial input must only contain POINT geometries.")

        coords <-
            input %>%
            sf::st_coordinates() %>%
            as_tibble
        xvar <- "X"
        yvar <- "Y"
    }

    if (!inherits(input, "sf")) {

        assert_that(has_name(input, xvar), has_name(input, yvar))

        input_old <- input

        input <-
            input[!is.na(input[,xvar]) & !is.na(input[,yvar]),]

        if (nrow(input) < nrow(input_old)) {
            warning(nrow(input_old) - nrow(input),
                    " locations were removed because of missing X or Y coordinates.")
        }

        coords <- input[, c(xvar, yvar)]

    }

    coords %>%
        dist %>%
        hclust(method = "complete") %>%
        cutree(h = max_dist) %>%
        tibble::enframe(name = NULL, value = output_var) %>%
        bind_cols(input, .)

}

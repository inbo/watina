#' Connect to the INBO Watina database
#'
#' Returns a connection to the INBO \strong{Watina} database.
#' The function can only be used from within the INBO network.
#'
#' Don't forget to disconnect at the end of your R-script using
#' \code{\link[DBI:dbDisconnect]{DBI::dbDisconnect()}}!
#'
#' @return
#' A \code{DBIConnection} object.
#'
#' @examples
#' \dontrun{
#' watina <- connect_watina()
#' # Do your stuff.
#' # Disconnect:
#' DBI::dbDisconnect(watina)
#' }
#'
#' @export
#' @importFrom inbodb connect_inbo_dbase
connect_watina <- function() {
    connect_inbo_dbase("W0002_00_Watina")
}


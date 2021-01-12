#' Connect to the INBO Watina database
#'
#' Returns a connection to the INBO \strong{Watina} database.
#' The function can only be used from within the INBO network.
#'
#' Don't forget to disconnect at the end of your R-script using
#' \code{\link{dbDisconnect}}!
#'
#' @return
#' A \code{DBIConnection} object.
#'
#' @examples
#' \dontrun{
#' watina <- connect_watina()
#' # Do your stuff.
#' # Disconnect:
#' dbDisconnect(watina)
#' }
#'
#' @export
#' @importFrom inbodb connect_inbo_dbase
connect_watina <- function() {
    connect_inbo_dbase("W0002_00_Watina",
                       autoconvert_utf8 = TRUE)
}



#' Disconnect a database connection
#'
#' This is a re-export of
#' \code{\link[inbodb:dbDisconnect-OdbcConnection-method]{inbodb::dbDisconnect()}}.
#'
#' @name dbDisconnect
#' @keywords documentation
#' @importFrom inbodb dbDisconnect
#' @export dbDisconnect
NULL

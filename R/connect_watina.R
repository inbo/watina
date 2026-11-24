#' Connect to the INBO Watina data warehouse
#'
#' Returns a connection to the INBO \strong{Watina} data warehouse.
#' The function can only be used from within the INBO network.
#'
#' Don't forget to disconnect at the end of your R-script using
#' \code{\link{dbDisconnect}}!
#'
#' @return
#' A \code{DBIConnection} object.
#'
#' @param scheduled_disconnect provides the option to schedule a database
#'        disconnect which will trigger at the end of an R session. This
#'        prevents open connections loading the server after this client
#'        has finished.
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
connect_watina <- function(scheduled_disconnect = FALSE) {
  con_watina <- inbodb::connect_inbo_dbase("W0002_10_Watina")

  # optionally register disconnect for finalization
  if (scheduled_disconnect) {
    schedule_disconnect(con_watina)
  }

  return(con_watina)
}


#' Disconnect a database connection
#'
#' This is a re-export of
#' \code{\link[inbodb:dbDisconnect-OdbcConnection-method]{inbodb::dbDisconnect()}}
#' (\href{https://inbo.github.io/inbodb/reference/dbDisconnect-OdbcConnection-method.html}{url}).
#'
#' @name dbDisconnect
#' @keywords documentation
#' @importFrom inbodb dbDisconnect
#' @export dbDisconnect
NULL


#' Disconnect a database object when the R session ends.
#'
#' This will schedule `dbDisconnect` for the moment that the global
#' environment is cleaned up at the end of an R session.
#' more info: `?reg.finalizer`
#'
#' @param con a database connection
#' @keywords internal
#'
schedule_disconnect <- function(con) {

  reg.finalizer(
    .GlobalEnv,
    function(e) {
      if (DBI::dbIsValid(con)) {
        DBI::dbDisconnect(con)
        message("WATINA database connection gracefully disconnected.")
      }
    },
    onexit = TRUE
  )

}

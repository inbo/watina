create_database_connection <- function(env = parent.frame()) {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  withr::defer(DBI::dbDisconnect(con),  envir = env)

  return(con)
}

mock_database_connection <- function(table, con = NULL, table_name = "temp_table", env = parent.frame()) {
  if (is.null(con)) {
    con <- create_database_connection(env = env)
  }

  DBI::dbWriteTable(con, table_name, table)
  withr::defer(DBI::dbRemoveTable(con, table_name),  envir = env)

  remote_table <- tbl(con, table_name)

  return(remote_table)
}

mock_data_from_db <- function(..., con = NULL, table_name = "temp_table", env = parent.frame()) {
  data <- tibble::tibble(...)

  remote_table <- mock_database_connection(
    table = data,
    con = con,
    table_name = table_name,
    env = env
  )

  return(remote_table)
}

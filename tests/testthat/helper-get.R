fetch_watina_connection <- function() {
  skip_if_not(
    exists("test_con", envir = .GlobalEnv),
    message = paste0(
      "No active database connection found. ",
      "Create connection by executing 'test_con <- connect_watina()'."
    )
  )

  return(get("test_con", envir = .GlobalEnv))
}

create_file_name <- function(function_name, test_name) {
  file_end <- "data.csv"

  paste0(function_name, "_", test_name, "_", file_end)
}

announce_files <- function(file_names, function_name) {
  for (f in file_names) {
    announce_snapshot_file(name = create_file_name(function_name, f))
  }
}

announce_files_locs <- function(file_names) {
  announce_files(file_names, "locs")
}

announce_files_xg3 <- function(file_names) {
  announce_files(file_names, "xg3")
}

announce_files_chem <- function(file_names) {
  announce_files(file_names, "chem")
}

announce_files_migration <- function(file_names) {
  announce_files(file_names, "dwh_migration")
}

clean_up_table <- function(table) {
  floor_0 <- c(
    "x",
    "y"
  )
  round_2 <- c(
    "soilsurf_ost",
    "measuringref_ost",
    "tubelength",
    "filterlength",
    "filterdepth"
  )

  table <- table %>%
    # Primary keys can change in DWH (exclude from comparison)
    select(-ends_with("_wid")) %>%
    mutate(
      # Round to only view big numerical changes in comparison view
      across(any_of(floor_0), floor),
      across(any_of(round_2), \(x) round(x, digits = 2))
    ) %>%
    # Order to ease comparison view
    arrange(
      pick(
        ends_with("_code"),
        ends_with("_id"),
        ends_with("year"),
        ends_with("date"),
        ends_with("_ost"),
        ends_with("_lcl"),
        ends_with("_variable")
      )
    )

  return(table)
}

write_file <- function(path, table) {
  write.csv(clean_up_table(table), path, row.names = FALSE)
}

expect_data <- function(data, function_name, test_name) {
  path <- tempfile(fileext = ".csv")
  write_file(path, data)
  expect_snapshot_file(path, create_file_name(function_name, test_name))
}

expect_locs <- function(locs, test_name) {
  expect_data(locs, "locs", test_name)
}

expect_xg3 <- function(xg3, test_name) {
  expect_data(xg3, "xg3", test_name)
}

expect_chem <- function(chem, test_name) {
  expect_data(chem, "chem", test_name)
}

expect_migration <- function(data, test_name) {
  expect_data(data, "dwh_migration", test_name)
}

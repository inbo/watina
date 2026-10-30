# Watina connection for testing ------------------------------------------------
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

# Clean up snapshot files ------------------------------------------------------
create_file_name <- function(function_name, test_name) {
  file_end <- "data.csv"

  paste0(function_name, "_", test_name, "_", file_end)
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
  round_3 <- c("value")
  round_5 <- c(
    "elneutr"
  )

  table <- table %>%
    # Primary keys can change in DWH (exclude from comparison)
    select(-ends_with("_wid")) %>%
    mutate(
      # Round to only view big numerical changes in comparison view
      across(any_of(floor_0), floor),
      across(matches("^[lhv]g3_"), \(x) {
        round(x, digits = 1)
      }),
      across(any_of(round_2), \(x) {
        round(x, digits = 2)
      }),
      across(any_of(round_3), \(x) {
        round(x, digits = 3)
      }),
      across(any_of(round_5), \(x) {
        round(x, digits = 5)
      })
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

# Announce snapshot files ------------------------------------------------------
# Assure snapshot files are not deleted when tests are skipped
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

# Expect snapshot tests --------------------------------------------------------
expect_data <- function(data, function_name, test_name) {
  path <- tempfile(fileext = ".csv")
  write.csv(clean_up_table(data), path, row.names = FALSE)
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

# Settings for DWH migration tests ---------------------------------------------
# Selected locations for DWH migration testing:
#   - location is locked (Status = 'afgesloten')
#   - with a sufficient amount of observations (COUNT(peilpunten) >= 1000)
dwh_test_locations <- c(
  "BLAP028",
  "BOBP020",
  "BGNP008",
  "BGNP009",
  "BGNP011",
  "BGNP012",
  "BGNP020",
  "BGNP021",
  "BGNP025",
  "BGNP028",
  "BGNP031",
  "BGNP035",
  "BGNP036",
  "CABP005",
  "BRSP005",
  "DUNS002",
  "DUNS003",
  "DURP004",
  "GRMP013",
  "GWZP010",
  "IJSP032",
  "KAMP002",
  "KAMP041",
  "KAMP206",
  "KAMS021",
  "KALP144",
  "KALP154",
  "KRGP038",
  "KRGP039",
  "POLP002",
  "POLP006",
  "KBRP128",
  "KBRS003",
  "KBRS007",
  "KBRS009",
  "UKPP025",
  "VLBP021",
  "VLBP023",
  "VLBS004",
  "ZSCP122",
  "ZSCP132",
  "ZSCP133",
  "ZWAS232"
)

# DWH W0002_10_Watina has only validity VLD
dwh_test_validity <- c("VLD")
# Include all loc_types
dwh_test_types <- c("P", "S", "R", "N", "W", "D", "L", "B")

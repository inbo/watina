test_that("Test to retrieve locations with get_locs", {
  file_name <- "locs_location_data.csv"
  announce_snapshot_file(name = file_name)
  skip_if(SKIP_DATA_VALIDATION_TESTS)

  watina <- connect_watina()
  locs <- get_locs(watina, area_codes = c("KAL", "ZWA"), loc_validity = "VLD")

  # Create a temp path
  path <- tempfile(fileext = ".csv")
  write.csv(locs, path, row.names = FALSE)

  dbDisconnect(watina)

  # Snapshot the file itself
  expect_snapshot_file(path, file_name)
})

test_that("Test to retrieve observation wells with get_locs", {
  file_name <- "locs_observation_wells_data.csv"
  announce_snapshot_file(name = file_name)
  skip_if(SKIP_DATA_VALIDATION_TESTS)

  watina <- connect_watina()
  obs <- get_locs(watina, area_codes = c("KAL", "ZWA"), loc_validity = "VLD", obswells = TRUE)

  # Create a temp path
  path <- tempfile(fileext = ".csv")
  write.csv(obs, path, row.names = FALSE)

  dbDisconnect(watina)

  # Snapshot the file itself
  expect_snapshot_file(path, file_name)
})

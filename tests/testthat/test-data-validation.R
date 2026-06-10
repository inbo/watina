# HELPER FUNCTIONS -------------------------------------------------------------
create_file_name <- function(test_name) {
  file_start <- "locs_"
  file_end <- "_data.csv"

  paste0(file_start, test_name,  file_end)
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

  floor_0 <- intersect(floor_0, colnames(table))
  round_2 <- intersect(round_2, colnames(table))

  table <- table %>%
    # Primary keys can change in DWH (exclude from comparison)
    select(-ends_with("_wid")) %>%
    mutate(
      # Round to only view big numerical changes in comparison view
      across(all_of(floor_0), floor),
      across(all_of(round_2), ~ round(.x, digits = 2))
    ) %>%
    # Order to ease comparison view
    arrange(pick(ends_with("_code")))

  return(table)
}

write_file <- function(path, table) {
  write.csv(clean_up_table(table), path, row.names = FALSE)
}

test_locs <- function(locs, test_name) {
  path <- tempfile(fileext = ".csv")
  write_file(path, locs)
  expect_snapshot_file(path, create_file_name(test_name))
}

# TEST -------------------------------------------------------------------------
test_that("Test different filters get_locs", {suppressWarnings({
  file_names <- c(
    "KAL_ZWA_locations",
    "KAL_ZWA_observation_wells",
    "bbox",
    "area_codes",
    "area_codes_loc_type",
    "loc_validity",
    "filterdepth_guess",
    "filterdepth_na",
    "loc_vec",
    "obswells",
    "obswell_aggr_latest",
    "obswell_aggr_latest_fd",
    "obswell_aggr_latest_sso",
    "obswell_aggr_mean",
    "mask"
  )

  for (f in  file_names) {
    announce_snapshot_file(name = create_file_name(f))
  }
  skip_if(SKIP_DATA_VALIDATION_TESTS)

  watina <- connect_watina(autoconvert_utf8 = FALSE)

  locs <- get_locs(watina, area_codes = c("KAL", "ZWA"), loc_validity = "VLD")
  test_locs(locs, "KAL_ZWA_locations")

  locs <- get_locs(watina, area_codes = c("KAL", "ZWA"), loc_validity = "VLD", obswells = TRUE)
  test_locs(locs, "KAL_ZWA_observation_wells")

  bbox <- c(xmin = 1.4e+5, xmax = 1.7e+5, ymin = 1.6e+5, ymax = 1.9e+5)
  locs <- get_locs(watina, bbox = bbox)
  test_locs(locs, "bbox")

  locs <- get_locs(watina, area_codes = c("KAL", "KBR"))
  test_locs(locs, "area_codes")

  locs <- get_locs(watina, area_codes = c("KAL", "KBR"), loc_type = c("P", "S"))
  test_locs(locs, "area_codes_loc_type")

  locs <- get_locs(watina, loc_validity = c("ENT", "DEL", "CLD"), loc_type = c("P", "S", "R", "N", "W", "D", "L", "B"))
  test_locs(locs, "loc_validity")

  locs <- get_locs(watina, area_codes = "WES", filterdepth_guess = TRUE)
  test_locs(locs, "filterdepth_guess")

  locs <- get_locs(watina, area_codes = c("KAL", "KBR"), loc_type = c("P", "S"), filterdepth_na = TRUE)
  test_locs(locs, "filterdepth_na")

  locs <- get_locs(watina, loc_vec = c("KBRP081", "KBRP090", "KBRP095", "KBRS001"))
  test_locs(locs, "loc_vec")

  locs <- get_locs(watina, obswells = TRUE, area_codes = c("KAL", "KBR"), loc_type = c("P", "S"))
  test_locs(locs, "obswells")

  locs <- get_locs(watina, area_codes = "WES", filterdepth_na = TRUE, filterdepth_guess = TRUE, obswell_aggr = "latest") %>%
    select(loc_code, contains("ost"), contains("filterdepth"))
  test_locs(locs, "obswell_aggr_latest")

  locs <- get_locs(watina, area_codes = "WES", filterdepth_na = TRUE, filterdepth_guess = TRUE, obswell_aggr = "latest_fd") %>%
    select(loc_code, contains("ost"), contains("filterdepth"))
  test_locs(locs, "obswell_aggr_latest_fd")

  locs <- get_locs(watina, area_codes = "WES", filterdepth_na = TRUE, filterdepth_guess = TRUE, obswell_aggr = "latest_sso") %>%
    select(loc_code, contains("ost"), contains("filterdepth"))
  test_locs(locs, "obswell_aggr_latest_sso")

  locs <- get_locs(watina, area_codes = "WES", filterdepth_na = TRUE, filterdepth_guess = TRUE, obswell_aggr = "mean") %>%
    select(loc_code, contains("ost"), contains("filterdepth"))
  test_locs(locs, "obswell_aggr_mean")

  mymask <-
    "https://geo.api.vlaanderen.be/VRBG/wfs" %>%
    httr::parse_url() %>%
    purrr::list_merge(query = list(
      request = "GetFeature",
      typeName = "VRBG:Refprv",
      cql_filter = "NAAM='West-Vlaanderen'",
      srsName = "EPSG:31370",
      outputFormat = "text/xml; subtype=gml/3.1.1"
    )) %>%
    httr::build_url() %>%
    sf::read_sf(crs = 31370) %>%
    sf::st_cast("GEOMETRYCOLLECTION")
  locs <- get_locs(watina, loc_validity = "VLD", mask = mymask, buffer = 0)
  test_locs(locs, "mask")

  dbDisconnect(watina)
})})

test_that("join_area_metadata joins and selects correctly", {
  con <- create_database_connection()

  meetpunt <- mock_data_from_db(
    MeetpuntWID = 1,
    MeetpuntCode = "LOC1",
    GebiedWID = c(1, 3, 5), # Necessary column
    con = con,
    table_name = "meetpunt"
  )
  gebied <- mock_data_from_db(
    GebiedWID = 1:6,
    GebiedCode = "AREA1",
    GebiedNaam = "Test Area",
    OtherCol = "Ignore Me", # This should be dropped by the select
    con = con,
    table_name = "gebied"
  )
  result <- join_area_metadata(meetpunt, gebied) %>% collect()

  expect_equal(nrow(result),  3)
  expect_equal(result$GebiedWID, c(1, 3, 5))
  expect_named(
    result,
    c("MeetpuntWID", "MeetpuntCode", "GebiedWID", "GebiedCode", "GebiedNaam"),
    ignore.order = TRUE)
})

test_that("filter_locations filters correcly", {
  locs <- mock_data_from_db(
    MeetpuntCode = c(
      "KALP001", "KALP002", "KALS001", "KALR001",
      "ZWAP001", "ZWAS001", "ZWAR001", "ZWAN001"
    ),
    GebiedCode = c(
      "KAL", "KAL", "KAL", "KAL",
      "ZWA", "ZWA", "ZWA", "ZWA"
    ),
    MeetpuntTypeCode = c(
      "P", "P", "S", "R",
      "P", "S", "R", "N"
    ),
    MeetpuntStatusCode = c(
      "VLD", "ENT", "DEL", "CLD",
      "VLD", "ENT", "DEL", "CLD"
    ),
    MeetpuntXCoordinaat = c(
      59999, 60000, 80000, 100000,
      120000, 140000, 160000, 160001
    ),
    MeetpuntYCoordinaat = c(
      80000, 80000, 80000, 139999,
      140000, 150000, 160000, 170000
    )
  )

  # loc_vec (MeetpuntCode)
  loc_vec <- c("KALP001", "KALP002", "KALP003", "KALP004")
  result <- locs %>% filter_by_location_code(loc_vec) %>% collect()
  expect_equal(result$MeetpuntCode, c("KALP001", "KALP002"))

  # area_codes (GebiedCode)
  area_codes <- "ZWA"
  result <- locs %>% filter_by_area_code(area_codes) %>% collect()
  expect_all_equal(result$GebiedCode, "ZWA")

  # bbox (MeetpuntXCoordinaat, MeetpuntYCoordrinaat)
  xmin <- 0.6e+5
  xmax <- 1.6e+5
  ymin <- 1.4e+5
  ymax <- 1.7e+5
  bbox <- c(xmin = xmin, xmax = xmax,  ymin = ymin,  ymax = ymax)
  result <- locs %>% filter_by_bbox(bbox) %>% collect()
  expect_true(all(dplyr::between(result$MeetpuntXCoordinaat, xmin, xmax)))
  expect_true(all(dplyr::between(result$MeetpuntYCoordinaat, ymin, ymax)))
  expect_true(nrow(result) == 3)

  # default filters: loc_type (MeetpuntTypeCode), loc_validity (MeetpuntStatusCode)
  loc_type <- "P"
  loc_validity <- c("VLD", "ENT")
  result <- locs %>% filter_locations(
    bbox = NULL,
    area_codes = NULL,
    loc_type = loc_type,
    loc_validity =  loc_validity,
    loc_vec = NULL
  ) %>% collect()
  expect_true(nrow(result) == 3)
  expect_all_equal(result$MeetpuntTypeCode, "P")
  expect_in(result$MeetpuntStatusCode, c("VLD", "ENT"))

  # Test combination of filters
  area_codes <- "ZWA"
  loc_vec <- c("ZWAP001", "ZWAS001", "ZWAN001")
  loc_type <- c("P", "S", "R")
  loc_validity <- "VLD"
  xmin <- 0.6e+5
  xmax <- 1.6e+5
  ymin <- 1.4e+5
  ymax <- 1.7e+5
  bbox <- c(xmin = xmin, xmax = xmax,  ymin = ymin,  ymax = ymax)
  result <- locs %>% filter_locations(
    bbox = bbox,
    area_codes = area_codes,
    loc_type = loc_type,
    loc_validity =  loc_validity,
    loc_vec = loc_vec
  ) %>% collect()
  expect_true(nrow(result) == 1)
  expect_mapequal(
    as.list(result),
    list(
      MeetpuntCode = "ZWAP001",
      GebiedCode = "ZWA",
      MeetpuntTypeCode = "P",
      MeetpuntStatusCode = "VLD",
      MeetpuntXCoordinaat = 120000,
      MeetpuntYCoordinaat = 140000
    )
  )
})

test_that("process_observation_wells", {
  peilpunt <- mock_data_from_db(
    PeilpuntPlaatsing = "2020-01-01",
    PeilpuntStopzetting = "2020-01-02",
    PeilpuntStatusCode = c(
      "VLD", "ENT", "CLD", "CLD",
      "DEL", "CLD", "VLD"
    ),
    PeilpuntOpenbaarheidTypeCode = c(
      "PLME", "PLME", "PLME", "PLME",
      "PLME", "CHME", "PLME"
    ),
    PeilpuntOpenbaarheidCode = c(
      "UNKWN", "UNKWN", "UNKWN", "UNKWN",
      "UNKWN", "UNKWN", "PUBL"
    ),
    PeilbuisLengte = c(
      -0.4, NA, 2.1, 1.5,
      0, 0, 0
    ),
    FilterLengte = c(
      NA, 0, 0.9, 0.8,
      0, 0, 0
    ),
    ReferentieNiveauMaaiveld = c
    (-0.2, NA, 1.4, 0.8,
      0, 0, 0
    ),
    ReferentieNiveauTAW = c(
      10, 50, NA, 90,
      0, 0, 0
    )
  )

  result <- peilpunt %>% process_observation_wells() %>% collect()

  expect_true(nrow(result) == 4) # last 3 rows deleted by filters
  expect_in(result$PeilpuntStatusCode, c("VLD", "ENT", "CLD"))
  expect_all_equal(result$PeilpuntOpenbaarheidTypeCode, "PLME")
  expect_all_equal(result$PeilpuntOpenbaarheidCode, "UNKWN")

  result <- result %>% compute_observation_metrics() %>% collect()

  expect_contains(
    colnames(result),
    c("tubelength", "filterlength", "filterdepth", "soilsurf_ost")
  )
  expect_equal(result$tubelength, c(NA, NA, 2.1, 1.5)) # Equal to Peilbuislengte, with negative numbers transformed to NA
  expect_equal(result$filterlength, c(0.3, 0.3, 0.9, 0.8)) # Equal to FilterLengte, with zero and NA transformed to 0.3
  expect_equal(result$filterdepth, c(NA, NA, 0.25, 0.3))
  expect_equal(result$soilsurf_ost, c(10.2, NA, NA, 89.2))
})

test_that("add_filterdepth_estimation_flag", {
  filterdepth <- c(NA, NA, NA, 1, 1)

  locs <- mock_data_from_db(
    filterdepth = filterdepth,
    filterdepth_original = filterdepth,
    tubelength = c(NA, 1, 2, NA, 2),
    filterlength = c(2, 1, 2, 1, 2)
  )

  result <- add_filterdepth_estimation_flag(locs) %>% collect()

  expect_contains(colnames(result), c("filterdepth_guessed"))
  expect_equal(result$filterdepth_guessed, c(0, 1, 1, 0, 0))
  expect_equal(result$filterdepth, c(NA, 0.5, 1, 1, 1))

  result_no_change <- result %>% filter(!result$filterdepth_guessed)
  expect_equal(
    result_no_change$filterdepth,
    result_no_change$filterdepth_original
  )
})

test_that("estimate_filterdepth", {
  locs <- mock_data_from_db(
    loc_typecode = c("P", "P", "P", "P", "P", "C", "C"),
    filterdepth = c(-1, 0, 3, 4, NA, -1, NA),
    # static values, extensive filterdepth_guess calculation in previous test
    tubelength = 2,
    filterlength = 2
  )

  result <- locs %>% estimate_filterdepth(
    filterdepth_range = c(0, 3),
    filterdepth_guess = TRUE,
    filterdepth_na = FALSE
  ) %>% collect()
  expect_contains(colnames(result), c("filterdepth_guessed"))

  result <- locs %>% estimate_filterdepth(
    filterdepth_range = c(0, 3),
    filterdepth_guess = FALSE,
    filterdepth_na = FALSE
  ) %>% collect()
  result_p <- result %>% filter(result$loc_typecode == "P")
  result_c <- result %>% filter(result$loc_typecode == "C")
  expect_disjoint(colnames(result), c("filterdepth_guessed"))
  expect_true(nrow(result) == 4)
  expect_equal(result_p$filterdepth, c(0, 3))
  expect_equal(result_c$filterdepth, c(-1,  NA))

  result <- locs %>% estimate_filterdepth(
    filterdepth_range = c(0, 3),
    filterdepth_guess = FALSE,
    filterdepth_na = TRUE
  ) %>% collect()
  result_p <- result %>% filter(result$loc_typecode == "P")
  result_c <- result %>% filter(result$loc_typecode == "C")
  expect_true(nrow(result) == 5)
  expect_equal(result_p$filterdepth, c(0, 3, NA))
  expect_equal(result_c$filterdepth, c(-1,  NA))
})

test_that("compute_observation_aggregations calculates group ranks and states correctly", {
  locs <- mock_data_from_db(
    loc_code = c("LOC1", "LOC1", "LOC1", "LOC2", "LOC3", "LOC3"),
    obswell_rank = c(1, 2, 3, 1, 1, 2),
    filterdepth = c(10, NA, NA, 5, NA, NA),
    soilsurf_ost = c(NA, 20, NA, 10, NA, NA),
    obswell_statecode = c("VLD", "ENT", "CLD", "VLD", "VLD", "ENT"),
    obswell_state = c("Valid", "Entered", "Closed", "Valid", "Valid", "Entered")
  )
  result <- compute_observation_aggregations(locs) %>% collect()

  res_loc1 <- result %>% filter(loc_code == "LOC1")
  res_loc2 <- result %>% filter(loc_code == "LOC2")
  res_loc3 <- result %>% filter(loc_code == "LOC3")

  # LOC1 - Some missing values for filters
  expect_all_equal(res_loc1$obswell_count, 3)
  expect_all_equal(res_loc1$obswell_maxrank, 3)
  expect_all_equal(res_loc1$obswell_maxrank_fd, 1)
  expect_all_equal(res_loc1$obswell_maxrank_sso, 2)
  expect_all_equal(res_loc1$obswell_statecode, "CLD")
  expect_all_equal(res_loc1$obswell_state, "Closed")

  # LOC2 - Baseline single entry (no change)
  expect_equal(res_loc2$loc_code, "LOC2")
  expect_equal(res_loc2$obswell_rank, 1)
  expect_equal(res_loc2$filterdepth, 5)
  expect_equal(res_loc2$soilsurf_ost, 10)
  expect_equal(res_loc2$obswell_statecode, "VLD")
  expect_equal(res_loc2$obswell_state, "Valid")

  # LOC3 - All missing values for filters
  expect_true(all(is.na(res_loc3$obswell_maxrank_fd)))
  expect_true(all(is.na(res_loc3$obswell_maxrank_sso)))
})

test_that("aggregate_guessed_flags", {
  # Scenario 1: Column doesn't exist
  locs <- mock_data_from_db(
    loc_code = "LOC1",
    filterdepth = 10
  )

  result <- aggregate_guessed_flags(locs)
  expect_equal(locs, result)

  # Scenario 2: Column exists
  locs <- mock_data_from_db(
    loc_code = c("LOC1", "LOC1", "LOC2", "LOC2"),
    filterdepth = 10,
    filterdepth_guessed = c(FALSE, TRUE, FALSE, FALSE)
  ) %>% group_by(loc_code)

  result <- aggregate_guessed_flags(locs) %>% collect()
  expect_true("filterdepth_guessed" %in% colnames(result))

  res_loc1 <- result %>% filter(loc_code == "LOC1")
  res_loc2 <- result %>% filter(loc_code == "LOC2")

  expect_all_equal(res_loc1$filterdepth_guessed, 1)
  expect_all_equal(res_loc2$filterdepth_guessed, 0)
})

test_that("aggregate_by_strategy correctly filters based on chosen strategy", {
  locs <- mock_data_from_db(
    row = 1:6,
    loc_code = c("LOC1", "LOC1", "LOC1", "LOC2", "LOC3", "LOC3"),
    obswell_rank = c(1, 2, 3, 1, 1, 2),
    filterdepth = c(10, NA, NA, 5, NA, NA),
    soilsurf_ost = c(NA, 20, NA, 10, NA, NA),
    measuringref_ost = c(1, 2, 3, 1, 2, 3),
    filterlength = c(1, 2, 3, 4, 5, 6),
    tubelength = c(1, 2, NA, NA, 2, 2),
    filterdepth_guessed = c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE),
    obswell_statecode = c("CLD", "CLD", "CLD", "VLD", "ENT", "ENT"),
    obswell_state = c("Closed", "Closed", "Closed", "Valid", "Entered", "Entered"),
    obswell_count = c(3, 3, 3, 1, 2, 2),
    obswell_maxrank = c(3, 3, 3, 1, 2, 2),
    obswell_maxrank_fd = c(1, 1, 1, 1, NA, NA),
    obswell_maxrank_sso = c(2, 2, 2, 1, NA, NA)
  ) %>% group_by(loc_code)

  result <- aggregate_by_strategy(locs, "latest") %>% collect()
  expect_equal(result$row,  c(3, 4, 6))

  result <- aggregate_by_strategy(locs, "latest_fd") %>% collect()
  expect_equal(result$row,  c(1, 4, 6))

  result <- aggregate_by_strategy(locs, "latest_sso") %>% collect()
  expect_equal(result$row,  c(2, 4, 6))

  result <- aggregate_by_strategy(locs, "mean") %>% collect()
  expect_equal(result$row,  c(3, 4, 6))
  expect_equal(result$soilsurf_ost,  c(20, 10, NA))
  expect_equal(result$measuringref_ost,  c(2, 1, 2.5))
  expect_equal(result$filterdepth,  c(10, 5, NA))
  expect_equal(result$filterlength,  c(2, 4, 5.5))
  expect_equal(result$tubelength,  c(1.5, NA, 2))
  expect_equal(result$filterdepth_guessed,  c(1, 1, 0))
})

test_that("drop_observation_metadata successfully drops obswell metadata", {
  locs <- mock_data_from_db(
    loc_code = "LOC1",
    obswell_code = "OBS1",
    obswell_rank = 1,
    obswell_installdate = Sys.Date(),
    obswell_stopdate = Sys.Date(),
    obswell_count = 1,
    obswell_maxrank = 1,
    obswell_maxrank_fd = 1,
    obswell_maxrank_sso = 1,
    keep_me = "Important Data"
  )

  result <- drop_observation_metadata(locs) %>% collect()

  expect_equal(colnames(result), c("loc_code", "keep_me"))
  expect_false(any(grepl("obswell_", colnames(result))))
})

create_testing_mask <- function() {
  coords <- matrix(
    c(
      50000, 170000,
      60000, 170000,
      55000, 180000,
      50000, 170000  # Closes the polygon
    ),
    ncol = 2,
    byrow = TRUE
  )

  poly <- sf::st_polygon(list(coords))
  sfc_geom <- sf::st_sfc(poly, crs = 31370)

  sf::st_sf(
    mask_column = "TESTING MASK",
    geometry = sfc_geom
  )
}

test_that("expand_mask", {
  mask <- create_testing_mask()

  expect_equal(expand_mask(mask, buffer =  0), mask)

  mask_expand <- expand_mask(mask, buffer = 10)
  expect_true(sf::st_area(mask_expand) > sf::st_area(mask))
})

test_that("execute_spatial_filter", {
  mask <- create_testing_mask()
  locs <- tibble::tibble(
    loc_code = c("LOC1", "LOC2"),
    x = c(55000, 200000),
    y = c(172000, 200000)
  ) %>% sf::st_as_sf(
    coords = c("x", "y"),
    crs = 31370,
    remove = FALSE
  )

  result <- locs %>% execute_spatial_filter(mask, join_mask = FALSE)
  expect_equal(result$loc_code, "LOC1")
  expect_null(attr(result, "sf_column"))
  expect_disjoint(colnames(result), "mask_column")

  result <- locs %>% execute_spatial_filter(mask, join_mask = TRUE)
  expect_equal(result$loc_code, "LOC1")
  expect_null(attr(result, "sf_column"))
  expect_contains(colnames(result), "mask_column")
})

test_that("filter_by_spatial_mask gives warning, filters and drops temperory columns", {
  mask <- create_testing_mask()
  locs <- mock_data_from_db(
    loc_wid = c(101, 102, 103), # Should be dropped by select()
    loc_code = c("LOC1", "LOC2", "LOC3"),
    area_code = c("A", "B", "A"),
    x = c(55000, 200000, NA),
    y = c(172000, 200000, 172000)
  )

  expect_warning(
    result <- filter_by_spatial_mask(locs = locs, mask = mask, join_mask = TRUE, buffer = 0),
    "Dropped 1 locations"
  )
  expect_equal(result$loc_code, "LOC1")
  expect_disjoint(colnames(result), "loc_wid")
  expect_contains(colnames(result), "mask_column")
  expect_null(attr(result, "sf_column"))
})

watina <- connect_watina(autoconvert_utf8 = FALSE)

sample_size_meetpunt <- 100
sample_size_peilpunt <- 500

gebied <- tbl(watina, "DimGebied") %>% collect()
saveRDS(gebied, file = test_path("fixtures", "dummy-data-gebied.rds"))

meetpunt <- tbl(watina, "DimMeetpunt") %>%
  filter(.data$GebiedWID %in% gebied$GebiedWID) %>%
  slice_sample(n = sample_size_meetpunt) %>%
  collect()
saveRDS(meetpunt, file = test_path("fixtures", "dummy-data-meetpunt.rds"))

peilpunt <- tbl(watina, "DimPeilpunt") %>%
  filter(.data$MeetpuntWID %in% meetpunt$MeetpuntWID) %>%
  slice_sample(n = sample_size_peilpunt) %>%
  collect()
saveRDS(peilpunt, file = test_path("fixtures", "dummy-data-peilpunt.rds"))

dbDisconnect(watina)

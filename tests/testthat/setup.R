withr::local_options(
  list(
    test.skip_snapshot = !(getOption("test.run_snapshot", default = FALSE)),
    test.skip_data_validation = !(getOption(
      "test.run_data_validation",
      default = FALSE
    )),
    test.skip_dwh_migration = !(getOption(
      "test.run_dwh_migration",
      default = FALSE
    ))
  ),
  .local_envir = testthat::teardown_env()
)

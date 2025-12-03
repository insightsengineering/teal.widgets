withr::local_options(
  list(
    shinytest2.timeout = 30 * 1000,
    shinytest2.load_timeout = 60 * 1000,
    shinytest2.duration = 0.5 * 1000
  ),
  .local_envir = testthat::test_env()
)

# Setup timeout options for shinytest2
withr::local_options(
  list(
    shinytest2.timeout = getOption(
      "shinytest2.timeout", default = Sys.getenv("SHINYTEST2_TIMEOUT", unset = 30 * 1000)
    ),
    shinytest2.load_timeout = getOption(
      "shinytest2.load_timeout", default = Sys.getenv("SHINYTEST2_LOAD_TIMEOUT", unset =  60 * 1000)
    ),
    shinytest2.duration = getOption(
      "shinytest2.duration", default = Sys.getenv("SHINYTEST2_DURATION", unset = 0.5 * 1000)
    )
  ),
  .local_envir = testthat::test_env()
)

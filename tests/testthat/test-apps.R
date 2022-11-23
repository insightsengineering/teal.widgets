
# tests the example apps in ./apps
# each app lives in its own directory, specific to the purpose of the test(s)
# the test specification lives under the app directory

# example:
# apps/pws_ggplot2 tests plot with settings click functionality with a ggplot2
# apps/tws tests table with settings: manipulating and downloading

# nolint start
skip_if_too_deep(5)
skip_on_cran()
skip_on_ci()

testthat::test_that("ggplot2_args, no error with empty inputs", {
  testthat::expect_error(ggplot2_args(), NA)
  testthat::expect_error(ggplot2_args(labs = list(), theme = list()), NA)
  testthat::expect_error(ggplot2_args(labs = list()), NA)
  testthat::expect_error(ggplot2_args(theme = list()), NA)
  testthat::expect_error(ggplot2_args(list()), NA)
  testthat::expect_error(ggplot2_args(list(), list()), NA)
})

# shinytest2::test_app(
#   app_dir = "tests/testthat/apps/tws", name = "app tests"
# )
# shinytest2::test_app(
#   app_dir = "tests/testthat/apps/pws_ggplot2", name = "app tests"
# )
# nolint end


# tests the example apps in ./apps
# each app lives in its own directory, specific to the purpose of the test(s)
# the test specification lives under the app directory

# example:
# apps/pws_click_base tests plot with settings click functionality with a base R
#   plot
# pws_click_base/app.R is the app used for testing
# pws_click_base/tests has the usual testthat file structure
# pws_click_base/tests/testthat/test-shinytest2.R is the test specification


skip_if_too_deep(5)
skip_on_cran()
skip_on_ci()

lapply(
  list.files("apps", full.names = TRUE),
  function(shiny_app_dir) {
    shinytest2::test_app(shiny_app_dir)
  }
)

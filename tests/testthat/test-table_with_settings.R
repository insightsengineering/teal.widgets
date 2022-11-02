library(shinytest2)

test_that("table-with-settings", {
  skip_on_cran()
  skip_on_ci()

  appdir <- system.file(package = "teal.widgets", "tws")
  shinytest2::test_app(appdir)
})

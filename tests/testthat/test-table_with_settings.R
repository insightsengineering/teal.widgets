library(shinytest2)

test_that("table with settings", {
  skip_on_cran()

  appdir <- system.file(package = "teal.widgets", "tws")
  shinytest2::test_app(appdir)
})

library(shinytest2)

test_that("sample_app works", {
  skip_on_cran()

  appdir <- system.file(package = "teal.widgets", "tws")
  shinytest2::test_app(appdir)
})

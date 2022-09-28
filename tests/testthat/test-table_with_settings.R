library(shinytest2)

test_that("sample_app works", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()

  appdir <- system.file(package = "teal.widgets", "table_with_settings")
  shinytest2::test_app(appdir)
})

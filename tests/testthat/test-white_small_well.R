testthat::test_that("Check that the class for white_small_well is correct", {
  dummy_white_small_well <- white_small_well()
  expected_class <- "shiny.tag.list"
  testthat::expect_s3_class(dummy_white_small_well, expected_class)
})

testthat::test_that("Snapshot test for white_small_well", {
  testthat::expect_snapshot(white_small_well())
})

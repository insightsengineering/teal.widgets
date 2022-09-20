testthat::test_that("optionalSliderInput single value", {
  testthat::expect_error(optionalSliderInput("a", "b", 0, 1, 0.2), NA)
})

testthat::test_that("optionalSliderInput single value - out of range", {
  testthat::expect_error(
    optionalSliderInput("a", "b", 0, 1, -1),
    "arguments inconsistent: min <= value <= max expected"
  )
})

testthat::test_that("optionalSliderInput double value", {
  testthat::expect_error(optionalSliderInput("a", "b", 0, 1, c(0.2, 0.8)), NA)
})

testthat::test_that("optionalSliderInput double value - out of range", {
  testthat::expect_error(
    optionalSliderInput("a", "b", 0, 1, c(-1, 2)),
    "arguments inconsistent: min <= value <= max expected"
  )
})

testthat::test_that("optionalSliderInput min/max NA", {
  testthat::expect_error(optionalSliderInput("a", "b", NA, 1, 0.2), NA)
  testthat::expect_error(optionalSliderInput("a", "b", NA, NA, 0.2), NA)
  testthat::expect_error(optionalSliderInput("a", "b", 0, 1, 0.2), NA)
})

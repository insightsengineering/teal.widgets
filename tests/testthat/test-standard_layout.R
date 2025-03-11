# Input validation ----
testthat::test_that("Input validation", {
  testthat::expect_silent(standard_layout(tags$div()))

  testthat::expect_error(
    standard_layout(NULL),
    regexp = "Assertion on 'output' failed"
  )

  testthat::expect_error(
    standard_layout(1),
    regexp = "Assertion on 'output' failed"
  )

  testthat::expect_error(standard_layout(
    output = tags$div(),
    encoding = 1
  ), regexp = "Assertion on 'encoding' failed")

  testthat::expect_error(standard_layout(
    output = tags$div(),
    pre_output = 1
  ), regexp = "Assertion on 'pre_output' failed")

  testthat::expect_error(standard_layout(
    output = tags$div(),
    post_output = 1
  ), regexp = "Assertion on 'post_output' failed")
})

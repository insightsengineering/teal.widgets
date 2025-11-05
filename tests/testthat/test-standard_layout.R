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

describe("Tests for standard_layout options", {
  mock_output <- shiny::plotOutput("test")
  mock_form <- shiny::actionButton("test", "")

  it("checks that the class is correct", {
    # Given
    expected_class <- "bslib_page"
    mock_layout <- standard_layout(output = mock_output, encoding = NULL, forms = mock_form)

    # Then
    expect_true(any(expected_class %in% class(mock_layout)))
  })

  it("checks snapshot with encoding and null forms", {
    # Given
    expected_class <- "bslib_page"
    mock_layout <- standard_layout(output = mock_output, encoding = mock_form, forms = NULL)

    # Then
    testthat::local_edition(3)
    expect_snapshot(as.character(mock_layout))
  })
})

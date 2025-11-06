testthat::test_that("optionalSliderInput single value", {
  testthat::expect_no_error(optionalSliderInput("a", "b", 0, 1, 0.2))
})

testthat::test_that("optionalSliderInput single value - out of range", {
  testthat::expect_error(
    optionalSliderInput("a", "b", 0, 1, -1),
    "arguments inconsistent: min <= value <= max expected"
  )
})

testthat::test_that("optionalSliderInput double value", {
  testthat::expect_no_error(optionalSliderInput("a", "b", 0, 1, c(0.2, 0.8)))
})

testthat::test_that("optionalSliderInput double value - out of range", {
  testthat::expect_error(
    optionalSliderInput("a", "b", 0, 1, c(-1, 2)),
    "arguments inconsistent: min <= value <= max expected"
  )
})

testthat::test_that("optionalSliderInput min/max NA", {
  testthat::expect_no_error(optionalSliderInput("a", "b", NA, 1, 0.2))
  testthat::expect_no_error(optionalSliderInput("a", "b", NA, NA, 0.2))
  testthat::expect_no_error(optionalSliderInput("a", "b", 0, 1, 0.2))
})

testthat::test_that("if inputId is not a string returns an error", {
  testthat::expect_error(optionalSelectInput(TRUE, "my label", c("choice_1", "choice_2")))
})

testthat::test_that("if inputId is not a string returns an error", {
  testthat::expect_error(optionalSelectInput(TRUE, "my label", c("choice_1", "choice_2")))
})

testthat::test_that("if label is not a string returns an error", {
  testthat::expect_error(optionalSelectInput("my_input_id", TRUE, c("choice_1", "choice_2")))
})

testthat::test_that("optionalSelectInput is a Shiny ui component", {
  testthat::expect_s3_class(
    optionalSelectInput(
      "my_input_id",
      "my label",
      c("choice_1", "choice_2"),
      label_help = shiny::helpText("This is a sample help text")
    ),
    "shiny.tag"
  )
})

testthat::test_that("if inputId is not a string optionalSliderInputValMinMax returns error", {
  testthat::expect_error(optionalSliderInputValMinMax(
    list,
    "label",
    c(5, 1, 10),
    label_help = shiny::helpText("Help")
  ))
})

testthat::test_that("value_min_max with invalid length throws error", {
  testthat::expect_error(optionalSliderInputValMinMax("id", "label", c(1, 2)))
})

testthat::test_that("value out of range in value_min_max throws error", {
  testthat::expect_error(
    optionalSliderInputValMinMax("id", "label", c(10, 1, 5)),
    "value_min_max"
  )
})

testthat::test_that("optionalSliderInputValMinMax is a Shiny ui component", {
  testthat::expect_s3_class(
    optionalSliderInputValMinMax(
      "id",
      "label",
      c(5, 1, 10),
      label_help = shiny::helpText("Help")
    ),
    "shiny.tag"
  )
})

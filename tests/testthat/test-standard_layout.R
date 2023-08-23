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

# Result ----
testthat::test_that("Basic output", {
  exp <- tags$div(
    class = "row",
    tags$div(
      class = "col-md-12",
      tags$div(
        class = "well",
        tags$div(class = "pre-output", NULL),
        tags$div(class = "output", tags$div()),
        tags$div(class = "post-output", NULL)
      ),
      NULL
    )
  )
  testthat::expect_equal(standard_layout(tags$div()), exp)

  testthat::expect_equal(
    standard_layout(output = tags$div(), pre_output = tags$div(), post_output = tags$div()),
    tags$div(
      class = "row",
      tags$div(
        class = "col-md-12",
        tags$div(
          class = "well",
          tags$div(class = "pre-output", tags$div()),
          tags$div(class = "output", tags$div()),
          tags$div(class = "post-output", tags$div())
        ),
        NULL
      )
    )
  )

  testthat::expect_equal(
    standard_layout(output = tags$div(), pre_output = tags$div(), post_output = tags$div(), forms = tags$div()),
    tags$div(
      class = "row",
      tags$div(
        class = "col-md-12",
        tags$div(
          class = "well",
          tags$div(class = "pre-output", tags$div()),
          tags$div(class = "output", tags$div()),
          tags$div(class = "post-output", tags$div())
        ),
        tags$div(class = "form-group", tags$div())
      )
    )
  )

  testthat::expect_equal(
    standard_layout(output = tags$div(), pre_output = tags$div(), post_output = tags$div(), encoding = tags$div()),
    tags$div(
      class = "row",
      tagList(
        tags$div(
          class = "col-md-3",
          tags$div(class = "well", tags$div()),
          NULL
        ),
        tags$div(
          class = "col-md-9",
          tags$div(
            class = "well",
            tags$div(class = "pre-output", tags$div()),
            tags$div(class = "output", tags$div()),
            tags$div(class = "post-output", tags$div())
          )
        )
      )
    )
  )

  testthat::expect_equal(
    standard_layout(
      output = tags$div(), pre_output = tags$div(),
      post_output = tags$div(), encoding = tags$div(), forms = div()
    ),
    tags$div(
      class = "row",
      tagList(
        tags$div(
          class = "col-md-3",
          tags$div(class = "well", tags$div()),
          tags$div(class = "form-group", tags$div())
        ),
        tags$div(
          class = "col-md-9",
          tags$div(
            class = "well",
            tags$div(class = "pre-output", tags$div()),
            tags$div(class = "output", tags$div()),
            tags$div(class = "post-output", tags$div())
          )
        )
      )
    )
  )
})

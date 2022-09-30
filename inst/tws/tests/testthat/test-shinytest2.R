library(shinytest2)

test_that("snapshotting inputs", {
  app <- AppDriver$new(name = "blah")

  app$expect_values(
    input = c(
      "table_with_settings-downbutton-file_format",
      "table_with_settings-downbutton-lpp",
      "table_with_settings-downbutton-pagination_switch"
    )
  )

  file_name <- app$get_value(input = "table_with_settings-downbutton-file_name")
  file_name <- sub("_\\d{6}$", "", file_name) # remove time
  expected_file_name <- paste0("table_", strftime(Sys.Date(), format = "%Y%m%d"))
  testthat::expect_equal(file_name, expected_file_name)

  app$expect_text("table")
  app$stop()
})

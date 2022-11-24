library(shinytest2)
library(testthat)

# launch app for all tests
testthat::test_that("launch app", {
  skip_if_too_deep(5)
  skip_on_ci()
  app <- AppDriver$new(name = "tws")
})

# Testing snapshots
testthat::test_that("file name and table content", {
  skip_if_too_deep(5)
  skip_on_ci()

  # default file name
  file_name <- app$get_value(input = "table_with_settings-downbutton-file_name")
  file_name <- sub("_\\d{6}$", "", file_name)
  expected_file_name <- paste0("table_", strftime(Sys.Date(),
    format = "%Y%m%d"
  ))
  testthat::expect_equal(file_name, expected_file_name)

  # check for table content
  app$expect_text("table")
})

# downloading tables
testthat::test_that("txt and csv download", {
  skip_if_too_deep(5)
  skip_on_ci()

  # download table in .csv
  app$click("table_with_settings-downbutton-dwnl")
  app$set_inputs(`table_with_settings-downbutton-dwnl_state` = TRUE)
  app$set_inputs(`table_with_settings-downbutton-file_format` = ".csv")
  app$set_inputs(`table_with_settings-downbutton-dwnl_state` = FALSE)
  app$set_inputs(`table_with_settings-downbutton-file_name` = "tab1")
  app$expect_download("table_with_settings-downbutton-data_download")

  # download table in .txt
  app$set_inputs(`table_with_settings-downbutton-file_format` = ".txt")
  app$set_inputs(`table_with_settings-downbutton-dwnl_state` = TRUE)
  app$set_inputs(`table_with_settings-downbutton-file_name` = "tab2")
  app$expect_download("table_with_settings-downbutton-data_download")
})

# stop app for all tests
testthat::test_that("close app", {
  skip_if_too_deep(5)
  skip_on_ci()

  app$stop()
})

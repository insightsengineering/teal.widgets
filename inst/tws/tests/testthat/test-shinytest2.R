library(shinytest2)

test_that("file download", {
  app <- AppDriver$new(name = "tws_download", height = 671, width = 1178)
  app$click("ddb")
  app$set_inputs(ddb_state = TRUE)
  app$set_inputs(`table_with_settings-downbutton-file_name` = "tab1")
  app$set_inputs(`table_with_settings-downbutton-lpp` = 80)
  app$expect_download("table_with_settings-downbutton-data_download")
  app$set_inputs(`table_with_settings-downbutton-file_format` = ".csv")
  app$set_inputs(ddb_state = FALSE)
  app$set_inputs(`table_with_settings-downbutton-file_name` = "tab2")
  app$expect_download("table_with_settings-downbutton-data_download")
  app$set_inputs(`table_with_settings-downbutton-file_format` = ".pdf")
  app$set_inputs(ddb_state = FALSE)
  app$stop()
})

test_that("snapshotting inputs", {
  app <- AppDriver$new(name = "tws_filename")
  file_name <- app$get_value(input = "table_with_settings-downbutton-file_name")
  file_name <- sub("_\\d{6}$", "", file_name)
  expected_file_name <- paste0("table_", strftime(Sys.Date(),
                                                  format = "%Y%m%d"))
  testthat::expect_equal(file_name, expected_file_name)
  app$expect_text("table")
  app$stop()
})

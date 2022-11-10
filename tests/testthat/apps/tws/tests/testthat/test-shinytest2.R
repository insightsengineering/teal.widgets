library(shinytest2)

test_that("{shinytest2} txt/csv download", {
  app <- AppDriver$new(name = "tws", height = 820, width = 1551)
  app$click("table_with_settings-downbutton-dwnl")
  app$set_inputs(`table_with_settings-downbutton-dwnl_state` = TRUE)
  app$set_inputs(`table_with_settings-downbutton-file_format` = ".csv")
  app$set_inputs(`table_with_settings-downbutton-dwnl_state` = FALSE)
  app$set_inputs(`table_with_settings-downbutton-file_name` = "tab1")
  app$expect_download("table_with_settings-downbutton-data_download")
  app$set_inputs(`table_with_settings-downbutton-file_format` = ".txt")
  app$set_inputs(`table_with_settings-downbutton-dwnl_state` = TRUE)
  app$set_inputs(`table_with_settings-downbutton-file_name` = "tab2")
  app$expect_download("table_with_settings-downbutton-data_download")
  app$set_inputs(`table_with_settings-downbutton-dwnl_state` = FALSE)
})

test_that("{shinytest2} snapshotting inputs", {
  app <- AppDriver$new(name = "tws_filename")
  file_name <- app$get_value(input = "table_with_settings-downbutton-file_name")
  file_name <- sub("_\\d{6}$", "", file_name)
  expected_file_name <- paste0("table_", strftime(Sys.Date(),
    format = "%Y%m%d"
  ))
  testthat::expect_equal(file_name, expected_file_name)
  app$expect_text("table")
  app$stop()
})

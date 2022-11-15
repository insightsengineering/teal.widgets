library(shinytest2)

test_that("{shinytest2} txt/csv download", {
  skip_on_cran()
  skip_on_ci()

  app <- AppDriver$new(name = "tws", height = 820, width = 1551)

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

  app$stop()
})

test_that("{shinytest2} snapshotting inputs", {
  skip_on_cran()
  skip_on_ci()

  app <- AppDriver$new(name = "tws_filename")

  # downloaded file name
  file_name <- app$get_value(input = "table_with_settings-downbutton-file_name")
  file_name <- sub("_\\d{6}$", "", file_name)
  expected_file_name <- paste0("table_", strftime(Sys.Date(),
    format = "%Y%m%d"
  ))
  testthat::expect_equal(file_name, expected_file_name)

  # check for table content
  app$expect_text("table")
  app$stop()
})

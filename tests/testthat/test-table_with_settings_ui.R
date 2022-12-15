library(shinytest2)

testthat::test_that("maxi version", {
  skip_on_cran()
  skip_on_ci()
  app <- AppDriver$new(
    app_tws(),
    name = "tws",
    variant = "app_tws_max",
    )
  # check standard file naming
  file_name <- app$get_value(input = "table_with_settings-downbutton-file_name")
  testthat::expect_equal(
    sub("_\\d{6}$", "", file_name),
    paste0("table_", strftime(Sys.Date(), format = "%Y%m%d"))
  )
  app$expect_text("table")
  app$set_window_size(width = 1392, height = 763)

  # save txt (default)
  app$click("table_with_settings-downbutton-dwnl")
  app$set_inputs(`table_with_settings-downbutton-dwnl_state` = TRUE)
  app$set_inputs(`table_with_settings-downbutton-file_name` = "table1")
  app$expect_download("table_with_settings-downbutton-data_download")

  # save csv:
  app$set_inputs(`table_with_settings-downbutton-file_format` = ".csv")
  app$set_inputs(`table_with_settings-downbutton-dwnl_state` = FALSE)
  app$set_inputs(`table_with_settings-downbutton-file_name` = "table2")
  app$expect_download("table_with_settings-downbutton-data_download")

  # download using modal, txt first
  app$click("table_with_settings-expand")
  app$set_inputs(`table_with_settings-modal_downbutton-lpp` = 70)
  app$click("table_with_settings-modal_downbutton-dwnl")
  app$set_inputs(`table_with_settings-modal_downbutton-file_format` = ".txt")
  app$set_inputs(`table_with_settings-modal_downbutton-pagination_switch` = FALSE)
  app$click("table_with_settings-modal_downbutton-dwnl")
  app$set_inputs(`table_with_settings-modal_downbutton-dwnl_state` = TRUE)
  app$set_inputs(`table_with_settings-modal_downbutton-file_name` = "table3")
  app$set_inputs(`table_with_settings-modal_downbutton-pagination_switch` = TRUE)
  # test pagination for at least one case
  app$set_inputs(`table_with_settings-modal_downbutton-lpp` = 1)
  app$set_inputs(`table_with_settings-modal_downbutton-lpp` = 11)
  app$expect_download("table_with_settings-modal_downbutton-data_download")

  # download csv using modal:
  app$set_inputs(`table_with_settings-modal_downbutton-file_format` = ".csv")
  app$set_inputs(`table_with_settings-modal_downbutton-dwnl_state` = FALSE)
  app$set_inputs(`table_with_settings-modal_downbutton-file_name` = "table4")
  app$expect_download("table_with_settings-modal_downbutton-data_download")

  # this compares the inputs and outputs stored in json:
  app$expect_values()
  # screenshot comparisons. Still experimenting whether there are values that will make it possible
  app$expect_screenshot(threshold = 100, kernel_size = 15, delay = 0.5)
  app$stop()
})


testthat::test_that("mini version", {
  skip_on_cran()
  skip_on_ci()
  app <- AppDriver$new(
    app_tws(),
    name = "tws",
    variant = "app_tws_mini",
  )
  file_name <- app$get_value(input = "table_with_settings-downbutton-file_name")
  testthat::expect_equal(
    sub("_\\d{6}$", "", file_name),
    paste0("table_", strftime(Sys.Date(), format = "%Y%m%d"))
  )
  app$expect_text("table")

  # click on download button
  app$click("table_with_settings-downbutton-dwnl")
  app$set_inputs(`table_with_settings-downbutton-file_name` = "table1")

  # test clicking on modal, no download
  app$click("table_with_settings-expand")
  app$click("table_with_settings-modal_downbutton-dwnl")
  app$set_inputs(`table_with_settings-modal_downbutton-file_name` = "table2")
  app$set_inputs(`table_with_settings-modal_downbutton-lpp` = 11)

  # and snapshots
  app$expect_screenshot(threshold = 100, kernel_size = 15, delay = 0.5)

  # now test values in json
  app$expect_values()
  app$stop()
})

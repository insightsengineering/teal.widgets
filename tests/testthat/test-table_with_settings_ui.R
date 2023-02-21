testthat::test_that("Table with settings: UI screenshots", {
  app <- shinytest2::AppDriver$new(
    app_tws(),
    name = "tws",
    variant = "app_tws_ui",
  )
  app$set_inputs(`table_with_settings-downbutton-file_name` = "table")

  # click on download button
  app$click("table_with_settings-downbutton-dwnl")

  # test clicking on modal
  app$click("table_with_settings-expand")
  # wait for the expand to happen
  Sys.sleep(0.1)
  app$set_inputs(`table_with_settings-modal_downbutton-lpp` = 70)
  app$click("table_with_settings-modal_downbutton-dwnl")
  app$set_inputs(`table_with_settings-modal_downbutton-file_name` = "table")

  # now test values in json
  app$expect_values(screenshot_args = FALSE, name = "final_values")
  app$stop()
})

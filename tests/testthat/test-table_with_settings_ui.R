library(shinytest2)

testthat::test_that("Table with settings: UI screenshots", {
  skip_on_cran()
  skip_on_ci()
  app <- AppDriver$new(
    app_tws(),
    name = "tws",
    variant = "app_tws_ui",
  )

  # set window size to be consistent among platforms
  app$set_window_size(width = 1400, height = 800)

  app$set_inputs(`table_with_settings-downbutton-file_name` = "table")

  # snapshot of initialized app
  app$expect_screenshot(threshold = 100, kernel_size = 15, delay = 0.4, name = "initial_view")

  # click on download button
  app$click("table_with_settings-downbutton-dwnl")

  # test clicking on modal
  app$click("table_with_settings-expand")
  app$click("table_with_settings-modal_downbutton-dwnl")
  app$set_inputs(`table_with_settings-modal_downbutton-file_name` = "table")

  # snapshots of modal opened
  app$expect_screenshot(threshold = 100, kernel_size = 15, delay = 0.4, name = "modal_view")

  # now test values in json
  app$expect_values(screenshot_args = FALSE, name = "final_values")
  app$stop()
})

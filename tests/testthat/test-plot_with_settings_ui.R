testthat::test_that("type_download_ui returns shiny.tag", {
  testthat::expect_s3_class(type_download_ui("STH"), "shiny.tag")
})

testthat::test_that("plot_with_settings_ui returns shiny.tag.list", {
  testthat::expect_s3_class(plot_with_settings_ui("STH"), "shiny.tag.list")
})

library(shinytest2)


testthat::test_that("Table with settings: UI screenshots", {
  skip_on_cran()
  skip_on_ci()
  app <- AppDriver$new(
    app_pws(),
    name = "pws",
    variant = "app_pws_ui"
  )

  app$click("button")
  app$expect_screenshot(threshold = 15, kernel_size = 10, delay = 0.5, name = "hidden")
  app$click("button")
  app$expect_screenshot(threshold = 15, kernel_size = 10, delay = 0.5, name = "visible")

  app$set_inputs(`plot_with_settings-downbutton-file_name` = "plot1")
  # now test values in json
  app$expect_values(screenshot_args = FALSE, name = "final_values")
  app$stop()
})

testthat::test_that("type_download_ui returns `shiny.tag`", {
  testthat::expect_s3_class(type_download_ui("STH"), "shiny.tag")
})

testthat::test_that("plot_with_settings_ui returns `shiny.tag.list`", {
  testthat::expect_s3_class(plot_with_settings_ui("STH"), "shiny.tag.list")
})

testthat::test_that("Plot with settings: UI screenshots", {
  app <- shinytest2::AppDriver$new(
    app_pws(),
    name = "pws",
    variant = "app_pws_ui"
  )
  threshold <- 75
  kernel_size <- 5
  delay <- 0.1

  app$set_window_size(width = 1000, height = 700)

  # click on hide/show button
  app$click("button")
  app$expect_screenshot(
    threshold = threshold, kernel_size = kernel_size, delay = delay, name = "hidden"
  )
  app$click("button")
  app$expect_screenshot(
    threshold = threshold, kernel_size = kernel_size, delay = delay, name = "visible"
  )

  app$set_inputs(`plot_with_settings-downbutton-file_name` = "plot1")
  app$set_inputs(`plot_with_settings-modal_downbutton-file_name` = "plot2")

  app$click("plot_with_settings-downbutton-downl")
  app$expect_screenshot(
    threshold = threshold, kernel_size = kernel_size, delay = delay, name = "download_menu"
  )
  app$click("plot_with_settings-expbut")
  app$expect_screenshot(
    threshold = threshold, kernel_size = kernel_size, delay = delay, name = "resize_menu"
  )

  app$stop()
})

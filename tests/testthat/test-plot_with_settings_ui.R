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
  app$wait_for_idle(timeout = default_idle_timeout)
  app$expect_screenshot(
    threshold = threshold, kernel_size = kernel_size, delay = delay, name = "hidden"
  )
  app$click("button")
  app$wait_for_idle(timeout = default_idle_timeout)
  app$expect_screenshot(
    threshold = threshold, kernel_size = kernel_size, delay = delay, name = "visible"
  )

  app$set_inputs(`plot_with_settings-downbutton-file_name` = "plot1")
  app$set_inputs(`plot_with_settings-modal_downbutton-file_name` = "plot2")

  app$click("plot_with_settings-downbutton-downl")
  app$wait_for_idle(timeout = default_idle_timeout)
  app$expect_screenshot(
    threshold = threshold, kernel_size = kernel_size, delay = delay, name = "download_menu"
  )
  app$click("plot_with_settings-expbut")
  app$wait_for_idle(timeout = default_idle_timeout)
  app$expect_screenshot(
    threshold = threshold, kernel_size = kernel_size, delay = delay, name = "resize_menu"
  )

  app$stop()
})

testthat::test_that(
  "e2e: teal.widgets::plot_with_settings is initialized with proper plot and buttons",
  {
    skip_if_too_deep(5)
    app <- shinytest2::AppDriver$new(
      app_pws(),
      name = "pws",
      variant = "app_pws_ui"
    )

    app$wait_for_idle(timeout = default_idle_timeout)

    # TODO: Check if there is an image.

    # Check if there are three buttons above the table.

    plot_buttons_selector <- "#plot_with_settings-plot-with-settings > div.plot-settings-buttons"
    plot_buttons <-
      app$get_html(plot_buttons_selector) %>%
      rvest::read_html() %>%
      rvest::html_elements("button")
    testthat::expect_length(plot_buttons, 3)

    # Check is the first one is a toggle button.
    testthat::expect_equal(
      plot_buttons[[1]] %>%
        rvest::html_attr("data-toggle"),
      "dropdown"
    )

    # Review the content of the toggle.
    testthat::expect_equal(
      app$get_text("#plot_with_settings-downbutton-file_format-label"),
      "File type"
    )

    testthat::expect_match(
      app$get_value(input = "plot_with_settings-downbutton-file_name"),
      sprintf("plot_%s", gsub("-", "", Sys.Date()))
    )

    # TODO - does it downloads the plot : P ? or do we need ot have it pre-downloaded for comparison
    # app$expect_download("#plot_with_settings-downbutton-data_download")

    # TODO - Check content of the second button.

    # Check content of the third button.

    app$click(selector = "#plot_with_settings-expbut")
    app$wait_for_idle(timeout = default_idle_timeout)

    testthat::expect_identical(
      app$get_text("#plot_with_settings-height-label"),
      "Plot height"
    )

    testthat::expect_identical(
      app$get_text("span.irs-min"),
      c("100", "250")
    )

    testthat::expect_identical(
      app$get_text("span.irs-max"),
      c("1,200", "750")
    )

    testthat::expect_identical(
      app$get_text("span.irs-single"),
      c("400", "500")
    )

    testthat::expect_identical(
      app$get_text("span.bootstrap-switch-handle-off.bootstrap-switch-default"),
      "OFF"
    )

    app$stop()
  }
)


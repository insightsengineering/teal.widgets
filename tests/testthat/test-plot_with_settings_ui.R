testthat::test_that("type_download_ui returns `shiny.tag`", {
  testthat::expect_s3_class(type_download_ui("STH"), "shiny.tag")
})

testthat::test_that("plot_with_settings_ui returns `shiny.tag.list`", {
  testthat::expect_s3_class(plot_with_settings_ui("STH"), "shiny.tag.list")
})

testthat::test_that("Plot with settings: UI screenshots", {
  app_driver <- shinytest2::AppDriver$new(
    app_driver_pws(),
    name = "pws",
    variant = "app_driver_pws_ui"
  )
  threshold <- 75
  kernel_size <- 5
  delay <- 0.1

  app_driver$set_window_size(width = 1000, height = 700)

  # click on hide/show button
  app_driver$click("button")
  app_driver$wait_for_idle(timeout = default_idle_timeout)
  app_driver$expect_screenshot(
    threshold = threshold, kernel_size = kernel_size, delay = delay, name = "hidden"
  )
  app_driver$click("button")
  app_driver$wait_for_idle(timeout = default_idle_timeout)
  app_driver$expect_screenshot(
    threshold = threshold, kernel_size = kernel_size, delay = delay, name = "visible"
  )

  app_driver$set_inputs(`plot_with_settings-downbutton-file_name` = "plot1")
  app_driver$set_inputs(`plot_with_settings-modal_downbutton-file_name` = "plot2")

  app_driver$click("plot_with_settings-downbutton-downl")
  app_driver$wait_for_idle(timeout = default_idle_timeout)
  app_driver$expect_screenshot(
    threshold = threshold, kernel_size = kernel_size, delay = delay, name = "download_menu"
  )
  app_driver$click("plot_with_settings-expbut")
  app_driver$wait_for_idle(timeout = default_idle_timeout)
  app_driver$expect_screenshot(
    threshold = threshold, kernel_size = kernel_size, delay = delay, name = "resize_menu"
  )

  app_driver$stop()
})

testthat::test_that(
  "e2e: teal.widgets::plot_with_settings is initialized with proper plot and buttons",
  {
    skip_if_too_deep(5)
    app_driver <- shinytest2::AppDriver$new(
      app_driver_pws(),
      name = "pws",
      variant = "app_driver_pws_ui"
    )

    app_driver$wait_for_idle(timeout = default_idle_timeout)

    # Check if there is an image.
    is_visible("#plot_with_settings-plot_main > img")

    # Check if there are three buttons above the table.

    plot_buttons_selector <- "#plot_with_settings-plot-with-settings > div.plot-settings-buttons"
    plot_buttons <-
      app_driver$get_html(plot_buttons_selector) %>%
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
      app_driver$get_text("#plot_with_settings-downbutton-file_format-label"),
      "File type"
    )

    testthat::expect_match(
      app_driver$get_value(input = "plot_with_settings-downbutton-file_name"),
      sprintf("plot_%s", gsub("-", "", Sys.Date()))
    )

    # Check content of the second button.
    app_driver$click(selector = "#plot_with_settings-expand")

    testthat::expect_identical(
      app_driver$get_value(input = "plot_with_settings-height_in_modal"),
      400
    )
    testthat::expect_identical(
      app_driver$get_text("#plot_with_settings-height_in_modal-label"),
      "Plot height"
    )
    testthat::expect_identical(
      app_driver$get_value(input = "plot_with_settings-width_in_modal"),
      500
    )
    testthat::expect_identical(
      app_driver$get_text("#plot_with_settings-width_in_modal-label"),
      "Plot width"
    )

    testthat::expect_identical(
      app_driver$get_text("span.irs-min"),
      c("100", "250")
    )

    testthat::expect_identical(
      app_driver$get_text("span.irs-max"),
      c("1,200", "750")
    )

    # already covered in
    # app_driver$get_value(input = "plot_with_settings-width_in_modal")
    # app_driver$get_value(input = "plot_with_settings-height_in_modal")
    testthat::expect_identical(
      app_driver$get_text("span.irs-single"),
      c("400", "500")
    )
    is_visible("#plot_with_settings-plot_main > img")

    # Check content of the third button.

    app_driver$click(selector = "#plot_with_settings-expbut")
    app_driver$wait_for_idle(timeout = default_idle_timeout)

    testthat::expect_identical(
      app_driver$get_text("#plot_with_settings-height-label"),
      "Plot height"
    )

    testthat::expect_identical(
      app_driver$get_text("span.irs-min"),
      c("100", "250")
    )

    testthat::expect_identical(
      app_driver$get_text("span.irs-max"),
      c("1,200", "750")
    )

    testthat::expect_identical(
      app_driver$get_text("span.irs-single"),
      c("400", "500")
    )

    testthat::expect_identical(
      app_driver$get_text("span.bootstrap-switch-handle-off.bootstrap-switch-default"),
      "OFF"
    )

    app_driver$stop()
  }
)


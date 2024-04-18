testthat::test_that("type_download_ui returns `shiny.tag`", {
  testthat::expect_s3_class(type_download_ui("STH"), "shiny.tag")
})

testthat::test_that("plot_with_settings_ui returns `shiny.tag.list`", {
  testthat::expect_s3_class(plot_with_settings_ui("STH"), "shiny.tag.list")
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
    testthat::expect_true(is_visible("#plot_with_settings-plot_main > img", app_driver))

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
    app_driver$wait_for_idle(timeout = default_idle_timeout)

    testthat::expect_identical(
      app_driver$get_value(input = "plot_with_settings-height_in_modal"),
      400L
    )
    testthat::expect_identical(
      app_driver$get_text("#plot_with_settings-height_in_modal-label"),
      "Plot height"
    )
    testthat::expect_identical(
      app_driver$get_value(input = "plot_with_settings-width_in_modal"),
      500L
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
    testthat::expect_true(is_visible("#plot_with_settings-plot_main > img", app_driver))

    # Check content of the third button.

    app_driver$click(selector = "#plot_with_settings-expbut")
    app_driver$wait_for_idle(timeout = default_idle_timeout)

    testthat::expect_identical(
      app_driver$get_text("#plot_with_settings-height-label"),
      "Plot height"
    )

    testthat::expect_identical(
      app_driver$get_text("span.irs-min"),
      rep(c("100", "250"), 2)
    )

    testthat::expect_identical(
      app_driver$get_text("span.irs-max"),
      rep(c("1,200", "750"), 2)
    )

    testthat::expect_identical(
      app_driver$get_text("span.irs-single"),
      rep(c("400", "500"), 2)
    )

    testthat::expect_identical(
      app_driver$get_text("span.bootstrap-switch-handle-off.bootstrap-switch-default"),
      "OFF"
    )

    app_driver$stop()
  }
)

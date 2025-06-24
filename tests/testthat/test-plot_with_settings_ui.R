#' Plot with settings app
#'
#' @description Example plot with setting app for testing using \code{shinytest2}
#'
#' @keywords internal
#'
app_driver_pws <- function() {
  shiny::shinyApp(
    ui = bslib::page_fluid(
      shiny::actionButton("button", "Show/Hide"),
      plot_with_settings_ui(
        id = "plot_with_settings"
      )
    ),
    server = function(input, output, session) {
      plot_r <- shiny::reactive({
        ggplot2::ggplot(data.frame(x = 1:5, y = 1:5)) +
          ggplot2::geom_point(ggplot2::aes(x = 1:5, y = 1:5))
      })

      show_hide_signal <- shiny::reactiveVal(TRUE)

      shiny::observeEvent(input$button, {
        show_hide_signal(
          !show_hide_signal()
        )
      })

      plot_data <- plot_with_settings_srv(
        id = "plot_with_settings",
        plot_r = plot_r,
        height = c(400, 100, 1200),
        width = c(500, 250, 750),
        brushing = TRUE,
        clicking = TRUE,
        dblclicking = TRUE,
        hovering = TRUE,
        show_hide_signal = show_hide_signal
      )

      shiny::exportTestValues(
        plot_r = plot_r,
        plot_data = plot_data
      )
    }
  )
}


# JS code to click the resize button popup.
# nolint start
click_resize_popup <- "// Select the element with the popover
                      const popoverTrigger = document.querySelector('i.fas.fa-maximize[data-bs-toggle=\"popover\"]');
                      // Initialize the popover if it isn't already initialized
                      const popover = bootstrap.Popover.getOrCreateInstance(popoverTrigger);
                      // Show the popover programmatically
                      popover.show();"

# JS code to click the expand button popup.
click_expand_popup <- "document.querySelector('#plot_with_settings-plot-with-settings > bslib-tooltip > button').click()"

# JS code to click the download button popup inside the expanded modal.
click_download_popup <- "// Select the element with the popover
                      const popoverTrigger = document.querySelector('i.fas.fa-download[data-bs-toggle=\"popover\"]');
                      // Initialize the popover if it isn't already initialized
                      const popover = bootstrap.Popover.getOrCreateInstance(popoverTrigger);
                      // Show the popover programmatically
                      popover.show();"
# nolint end

get_active_module_pws_output <- function(app_driver, pws, attr) {
  testthat::skip_if_not_installed("rvest")
  app_driver$get_html("html") %>%
    rvest::read_html() %>%
    rvest::html_elements(sprintf("#plot_with_settings-%s > img", pws)) %>%
    rvest::html_attr(attr)
}

is_scrollable <- function(app, selector) {
  js_is_scrollable <- "function isScrollable(element) {

    const hasVerticalScroll = element.scrollHeight > element.clientHeight;
    const hasHorizontalScroll = element.scrollWidth > element.clientWidth;

    return {
      vertical: hasVerticalScroll,
      horizontal: hasHorizontalScroll,
      any: hasVerticalScroll || hasHorizontalScroll
    };
  };"


  element <- sprintf("const targetElement = document.querySelector('%s');", selector)
  js <- paste(js_is_scrollable, element, "isScrollable(targetElement);", collapse = "\n")
  app$get_js(js)
}

testthat::test_that("type_download_ui returns `shiny.tag`", {
  testthat::expect_s3_class(type_download_ui("STH"), "shiny.tag")
})

testthat::test_that("plot_with_settings_ui returns `shiny.tag`", {
  testthat::expect_s3_class(plot_with_settings_ui("STH"), "shiny.tag")
})

testthat::test_that(
  "e2e: teal.widgets::plot_with_settings: initializes with a plot and the settings buttons",
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

    # Check if the settings buttons are visible.
    testthat::expect_true(is_visible(".teal-widgets.settings-buttons", app_driver))

    app_driver$stop()
  }
)

testthat::test_that(
  "e2e: teal.widgets::plot_with_settings: buttons have proper FA icons and two of them are dropdowns",
  {
    skip_if_too_deep(5)
    testthat::skip_if_not_installed("rvest")
    app_driver <- shinytest2::AppDriver$new(
      app_driver_pws(),
      name = "pws",
      variant = "app_driver_pws_ui"
    )
    app_driver$wait_for_idle(timeout = default_idle_timeout)

    app_driver$run_js(click_download_popup)
    app_driver$wait_for_idle(timeout = default_idle_timeout)

    testthat::expect_equal(
      app_driver$get_text("#plot_with_settings-downbutton-file_format-label"),
      "File type"
    )
    testthat::expect_identical(
      app_driver$get_value(input = "plot_with_settings-downbutton-file_format"),
      "png"
    )

    file_format_text <- app_driver$get_text("#plot_with_settings-downbutton-file_format > div")
    testthat::expect_match(file_format_text, "svg\n", fixed = TRUE)
    testthat::expect_match(file_format_text, "pdf\n", fixed = TRUE)

    testthat::expect_equal(
      app_driver$get_text("#plot_with_settings-downbutton-file_name-label"),
      "File name (without extension)"
    )
    testthat::expect_match(
      app_driver$get_value(input = "plot_with_settings-downbutton-file_name"),
      sprintf("plot_%s", gsub("-", "", Sys.Date()))
    )

    testthat::expect_true(is_visible("#plot_with_settings-downbutton-data_download", app_driver))

    download_button <-
      app_driver$get_html("#plot_with_settings-downbutton-data_download > i") %>%
      rvest::read_html()

    testthat::expect_equal(
      download_button %>%
        rvest::html_element("i") %>%
        rvest::html_attr("class"),
      "fas fa-download"
    )
    testthat::expect_equal(
      download_button %>%
        rvest::html_element("i") %>%
        rvest::html_attr("aria-label"),
      "download icon"
    )

    app_driver$stop()
  }
)

testthat::test_that(
  "e2e: teal.widgets::plot_with_settings: the click on the expand button opens an overlay
  plot height, plot width, plot, download dropdown and dismiss button",
  {
    skip_if_too_deep(5)
    app_driver <- shinytest2::AppDriver$new(
      app_driver_pws(),
      name = "pws",
      variant = "app_driver_pws_ui",
      height = 1000,
      width = 1000
    )
    app_driver$wait_for_idle(timeout = default_idle_timeout)
    app_driver$get_text(paste0(
      "#plot_with_settings-plot-with-settings > div > div > ",
      "div.teal-widgets.settings-buttons > bslib-tooltip.resize-button > div:nth-child(1)"
    ))
    pre_click <- app_driver$get_values()
    testthat::expect_false(app_driver$get_value(input = "plot_with_settings-plot-with-settings_full_screen"))

    app_driver$set_inputs(`plot_with_settings-expbut` = TRUE)

    # Expand the plot and evaluate the output
    app_driver$run_js(click_expand_popup, timeout = default_idle_timeout)
    testthat::expect_true(is_visible("#bslib-full-screen-overlay", app_driver))

    # Resize button
    testthat::expect_true(is_visible("#plot_with_settings-expbut > i", app_driver))

    # Download button
    testthat::expect_true(is_visible(paste0(
      "#plot_with_settings-plot-with-settings > div > div > div.teal-widgets.settings-buttons",
      " > bslib-tooltip.download-button > div:nth-child(2) > bslib-popover > i"
    ), app_driver))

    # Dismiss button
    testthat::expect_equal(app_driver$get_text("#bslib-full-screen-overlay"), "Close ")
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e: teal.widgets::plot_with_settings: the click on the download button in expand modal opens a download dropdown",
  {
    skip_if_too_deep(5)
    app_driver <- shinytest2::AppDriver$new(
      app_driver_pws(),
      name = "pws",
      variant = "app_driver_pws_ui"
    )
    app_driver$wait_for_idle(timeout = default_idle_timeout)

    app_driver$run_js(click_download_popup)
    app_driver$wait_for_idle(timeout = default_idle_timeout)
    testthat::expect_true(is_visible("#plot_with_settings-plot_main > img", app_driver))

    testthat::expect_equal(
      app_driver$get_text("#plot_with_settings-downbutton-file_format-label"),
      "File type"
    )
    testthat::expect_identical(
      app_driver$get_value(input = "plot_with_settings-downbutton-file_format"),
      "png"
    )

    testthat::expect_equal(
      app_driver$get_text("#plot_with_settings-downbutton-file_name-label"),
      "File name (without extension)"
    )
    testthat::expect_match(
      app_driver$get_value(input = "plot_with_settings-downbutton-file_name"),
      sprintf("plot_%s", gsub("-", "", Sys.Date()))
    )

    app_driver$stop()
  }
)

testthat::test_that(
  "e2e: teal.widgets::plot_with_settings: the click on the resize button opens a dropdown menu
  plot height, plot width, plot, download dropdown and dismiss button",
  {
    skip_if_too_deep(5)
    app_driver <- shinytest2::AppDriver$new(
      app_driver_pws(),
      name = "pws",
      variant = "app_driver_pws_ui",
      height = 1000,
      width = 1000
    )
    app_driver$wait_for_idle(timeout = default_idle_timeout)

    testthat::expect_false(is_visible("#plot_with_settings-slider_ui", app_driver))

    # nolint start
    app_driver$run_js(click_resize_popup)
    # nolint end
    app_driver$wait_for_idle(timeout = default_idle_timeout)


    testthat::expect_identical(
      app_driver$get_value(input = "plot_with_settings-height"),
      400L
    )
    testthat::expect_identical(
      app_driver$get_text("#plot_with_settings-height-label"),
      "Plot height"
    )
    testthat::expect_identical(
      app_driver$get_value(input = "plot_with_settings-width"),
      500L
    )

    testthat::expect_identical(
      app_driver$get_text("#plot_with_settings-slider_ui > div > div:nth-child(3) > div > label > span"),
      "Automatic"
    )

    app_driver$stop()
  }
)


testthat::test_that(
  "e2e: teal.widgets::plot_with_settings: it is possible to set height and width for the plot
  on the third button dropdown menu without errors",
  {
    skip_if_too_deep(5)
    app_driver <- shinytest2::AppDriver$new(
      app_driver_pws(),
      name = "pws",
      variant = "app_driver_pws_ui"
    )
    app_driver$wait_for_idle(timeout = default_idle_timeout)

    app_driver$set_inputs(`plot_with_settings-height` = 1000)
    app_driver$set_inputs(`plot_with_settings-width_resize_switch` = 350)

    testthat::expect_null(
      app_driver$get_html(".shiny-output-error-validation"),
      info = "No validation error is observed"
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e teal.widgets::plot_with_settings: clicking download+download button downloads image in a specified format",
  {
    skip_if_too_deep(5)
    app_driver <- shinytest2::AppDriver$new(
      app_driver_pws(),
      name = "pws",
      variant = "app_driver_pws_ui"
    )
    app_driver$wait_for_idle(timeout = default_idle_timeout)

    app_driver$run_js(click_download_popup)
    app_driver$wait_for_idle(timeout = default_idle_timeout)

    filename <- app_driver$get_download("plot_with_settings-downbutton-data_download")
    testthat::expect_match(filename, "png$", fixed = FALSE)

    app_driver$stop()
  }
)

testthat::test_that("e2e teal.widgets::plot_with_settings: expanded image can be resized", {
  skip_if_too_deep(5)
  app_driver <- shinytest2::AppDriver$new(
    app_driver_pws(),
    name = "pws",
    variant = "app_driver_pws_ui",
    height = 1000,
    width = 1000
  )
  app_driver$wait_for_idle(timeout = default_idle_timeout)

  app_driver$run_js(click_expand_popup)
  app_driver$wait_for_idle(timeout = default_idle_timeout)

  plot_before <- get_active_module_pws_output(app_driver, pws = "plot_modal", attr = "src")
  values <- app_driver$get_values()
  testthat::expect_equal(
    values$output$`plot_with_settings-plot_main`$width,
    500L
  )

  testthat::expect_equal(
    values$output$`plot_with_settings-plot_main`$height,
    400L
  )
  app_driver$run_js(click_resize_popup)
  app_driver$wait_for_idle(timeout = default_idle_timeout)

  app_driver$set_inputs(`plot_with_settings-height` = 1000)
  app_driver$set_inputs(`plot_with_settings-width` = 350)
  app_driver$wait_for_idle(timeout = default_idle_timeout)
  values_resized <- app_driver$get_values()

  testthat::expect_equal(
    values_resized$output$`plot_with_settings-plot_main`$width,
    350L
  )

  testthat::expect_equal(
    values_resized$output$`plot_with_settings-plot_main`$height,
    1000L
  )

  testthat::expect_false(
    identical(
      values$output$`plot_with_settings-plot_main`$src,
      values_resized$output$`plot_with_settings-plot_main`$src
    )
  )

  app_driver$stop()
})

testthat::test_that("e2e teal.widgets::plot_with_settings: expanded image can be downloaded", {
  skip_if_too_deep(5)
  app_driver <- shinytest2::AppDriver$new(
    app_driver_pws(),
    name = "pws",
    variant = "app_driver_pws_ui"
  )
  app_driver$wait_for_idle(timeout = default_idle_timeout)

  app_driver$run_js(click_expand_popup)
  app_driver$wait_for_idle(timeout = default_idle_timeout)

  app_driver$run_js(click_download_popup)
  app_driver$wait_for_idle(timeout = default_idle_timeout)

  filename <- app_driver$get_download("plot_with_settings-downbutton-data_download")
  testthat::expect_match(filename, "png$", fixed = FALSE)

  app_driver$stop()
})

testthat::test_that("e2e teal.widgets::plot_with_settings: main image can be resized", {
  skip_if_too_deep(5)
  app_driver <- shinytest2::AppDriver$new(
    app_driver_pws(),
    name = "pws",
    variant = "app_driver_pws_ui"
  )
  app_driver$wait_for_idle(timeout = default_idle_timeout)

  app_driver$run_js(click_resize_popup)
  app_driver$wait_for_idle(timeout = default_idle_timeout)

  plot_before <- get_active_module_pws_output(app_driver, pws = "plot_main", attr = "src")

  testthat::expect_equal(
    get_active_module_pws_output(app_driver, pws = "plot_main", attr = "width"),
    "500"
  )

  testthat::expect_equal(
    get_active_module_pws_output(app_driver, pws = "plot_main", attr = "height"),
    "400"
  )

  app_driver$set_inputs(`plot_with_settings-height` = 1000)
  app_driver$set_inputs(`plot_with_settings-width` = 350)

  testthat::expect_equal(
    get_active_module_pws_output(app_driver, pws = "plot_main", attr = "width"),
    "350"
  )

  testthat::expect_equal(
    get_active_module_pws_output(app_driver, pws = "plot_main", attr = "height"),
    "1000"
  )

  testthat::expect_false(
    identical(plot_before, get_active_module_pws_output(app_driver, pws = "plot_main", attr = "src"))
  )

  app_driver$stop()
})

testthat::test_that("e2e teal.widgets::plot_with_settings: scrollbar appears when image is resized", {
  skip_if_too_deep(5)
  app_driver <- shinytest2::AppDriver$new(
    app_driver_pws(),
    name = "pws",
    variant = "app_driver_pws_ui"
  )
  app_driver$wait_for_idle(timeout = default_idle_timeout)

  app_driver$run_js(click_expand_popup)
  app_driver$wait_for_idle(timeout = default_idle_timeout)
  app_driver$run_js(click_resize_popup)
  app_driver$wait_for_idle(timeout = default_idle_timeout)

  app_driver$set_inputs(`plot_with_settings-height` = 10000)
  app_driver$set_inputs(`plot_with_settings-width` = 350)
  scrollable <- is_scrollable(app_driver, ".card-body.html-fill-container")
  testthat::expect_true(scrollable$any)
  testthat::expect_true(scrollable$vertical)

  app_driver$stop()
})

library(shinytest2)

brush_vals <- c(
  1.575922029932, 2.2959573557845,
  1.218473025872, 2.2972720836904, 252, 532, 251, 349, 252, 532, 251, 349, 1,
  1, "1:5", "1:5", 1, 5, 1, 5, 28.0413099315069, 1583.52054794521, 368.846473150198,
  5.47945205479452, character(0), character(0), "xy", "plot_with_settings-plot_brush",
  "plot_with_settings-plot_main"
)

hover_vals <- c(
  2.95684692272772, 2.34130469829519,
  789, 247, 789, 247, 1, 1, "1:5", "1:5", 1, 5, 1, 5, 28.0413099315069, 1583.52054794521,
  368.846473150198, 5.47945205479452, character(0), character(0)
)

dbl_click_vals <- c(
  2.95684692272772, 2.34130469829519,
  789, 247, 789, 247, 1, 1, "1:5", "1:5", 1, 5, 1, 5, 28.0413099315069, 1583.52054794521,
  368.846473150198, 5.47945205479452, character(0), character(0)
)

click_vals <- c(
  2.95941847746291, 1.9450111668517,
  790, 283, 790, 283, 1, 1, "1:5", "1:5", 1, 5, 1, 5, 28.0413099315069, 1583.52054794521,
  368.846473150198, 5.47945205479452, character(0), character(0)
)

# one app opens for all tests
pws <- app_pws()
app <- AppDriver$new(pws, name = pws, height = 937, width = 1619)

# tests plot click functionalities
testthat::test_that("plot_with_settings: click functionalities ggplot2", {
  skip_on_cran()
  skip_on_ci()

  # hovering
  app$set_inputs(
    `plot_with_settings-plot_hover` = hover_vals,
    allow_no_input_binding_ = TRUE, priority_ = "event"
  )

  # double click
  app$set_inputs(
    `plot_with_settings-plot_dblclick` = dbl_click_vals,
    allow_no_input_binding_ = TRUE, priority_ = "event"
  )

  # click
  app$set_inputs(
    `plot_with_settings-plot_click` = click_vals,
    allow_no_input_binding_ = TRUE, priority_ = "event"
  )

  # brushing
  app$set_inputs(
    `plot_with_settings-plot_brush` = brush_vals, allow_no_input_binding_ = TRUE
  )

  vals <- app$get_values()

  # testing clicking / hovering / brushing
  test_hover <- shiny::isolate(vals$export$plot_data$hover())
  testthat::expect_equal(
    test_hover,
    hover_vals
  )

  test_dblclick <- shiny::isolate(vals$export$plot_data$dblclick())
  testthat::expect_equal(
    test_dblclick,
    dbl_click_vals
  )

  test_click <- shiny::isolate(vals$export$plot_data$click())
  testthat::expect_equal(
    test_click,
    click_vals
  )

  test_brush <- shiny::isolate(vals$export$plot_data$brush())
  testthat::expect_equal(
    test_brush,
    brush_vals
  )

  # reset brush to character(0) for next tests
  app$set_inputs(
    `plot_with_settings-plot_brush` = character(0),
    allow_no_input_binding_ = TRUE
  )
})

# test output that is returned (reactives and graphic encoded in base64)
testthat::test_that("plot_with_settings: output is returned", {
  skip_on_cran()
  skip_on_ci()

  vals <- app$get_values()

  # check if outputs are reactive
  for (react_i in vals$export$"plot_data") {
    testthat::expect_true(is(react_i, "reactive"))
  }

  # check if output is desired plot
  testthat::expect_type(
    vals$output$`plot_with_settings-plot_main`$src, "character"
  )
  # nolint start
  testthat::expect_true(
    grepl("data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAfQAAAGQCAIAAADX",
      vals$output$`plot_with_settings-plot_main`$src,
      fixed = TRUE
    )
  )
  # nolint end
})

# download plots. expect_download() might not be stable, hence we test
# setting inputs and plot name changes
testthat::test_that("plot_with_settings: download functionality ggplot2", {
  skip_on_cran()
  skip_on_ci()

  # test default download options
  app$click("plot_with_settings-downbutton-downl")
  date <- strftime(Sys.time(), format = "%Y%m%d")
  testthat::expect_true(
    grepl(
      paste0("plot_", date),
      app$get_value(input = "plot_with_settings-downbutton-file_name")
    )
  )
  testthat::expect_equal(
    app$get_value(input = "plot_with_settings-downbutton-file_format"), "png"
  )

  # Download svg format
  app$set_inputs(`plot_with_settings-downbutton-downl_state` = TRUE)
  app$set_inputs(`plot_with_settings-downbutton-file_format` = "svg")
  app$set_inputs(`plot_with_settings-downbutton-file_name` = "plot_svg")
  app$click("plot_with_settings-downbutton-downl")
  testthat::expect_equal(
    app$get_value(input = "plot_with_settings-downbutton-file_name"), "plot_svg"
  )

  # change plot width and height
  app$click("plot_with_settings-expbut")
  app$set_inputs(`plot_with_settings-width` = 300)
  app$set_inputs(`plot_with_settings-height` = 300)
  app$click("plot_with_settings-downbutton-downl")
  vals <- app$get_values()

  testthat::expect_equal(app$get_value(input = "plot_with_settings-downbutton-file_name"), "plot_svg")
  testthat::expect_equal(isolate(vals$output$`plot_with_settings-plot_main`$height), 300)
  testthat::expect_equal(isolate(vals$output$`plot_with_settings-plot_main`$width), 300)

  # reset
  app$set_inputs(`plot_with_settings-expbut_state` = FALSE)
  app$set_inputs(`plot_with_settings-downbutton-downl_state` = FALSE)
})

# downloading plot with modal
testthat::test_that("plot_with_settings: download ggplot2 modal", {
  skip_on_cran()
  skip_on_ci()

  # default downloading with modal
  app$set_inputs(
    `plot_with_settings-plot_hover` = character(0),
    allow_no_input_binding_ = TRUE
  )
  app$click("plot_with_settings-expand")
  app$click("plot_with_settings-modal_downbutton-downl")
  date <- strftime(Sys.time(), format = "%Y%m%d")
  testthat::expect_true(
    grepl(
      paste0("plot_", date),
      app$get_value(input = "plot_with_settings-modal_downbutton-file_name")
    )
  )

  # Change width and height of plot on modal
  app$click("plot_with_settings-expand")
  app$click("plot_with_settings-modal_downbutton-downl")
  app$set_inputs(`plot_with_settings-height_in_modal` = 400)
  app$set_inputs(`plot_with_settings-width_in_modal` = 500)
  app$set_inputs(`plot_with_settings-modal_downbutton-file_name` = "plot_pdf")
  app$set_inputs(`plot_with_settings-modal_downbutton-file_format` = "pdf")
  app$set_inputs(`plot_with_settings-height_in_modal` = 1200)
  app$set_inputs(`plot_with_settings-width_in_modal` = 750)
  app$click("plot_with_settings-modal_downbutton-downl")
  app$set_inputs(`plot_with_settings-modal_downbutton-downl_state` = TRUE)

  vals <- app$get_values()
  testthat::expect_equal(
    shiny::isolate(vals$input$`plot_with_settings-modal_downbutton-file_name`),
    "plot_pdf"
  )
  testthat::expect_equal(
    shiny::isolate(vals$input$`plot_with_settings-height_in_modal`), 1200
  )
  testthat::expect_equal(
    shiny::isolate(vals$input$`plot_with_settings-width_in_modal`), 750
  )

  # nolint start
  testthat::expect_equal(
    shiny::isolate(vals$output$`plot_with_settings-plot_modal`$width), 750
  )
  testthat::expect_equal(
    shiny::isolate(vals$output$`plot_with_settings-plot_modal`$height), 1200
  )
  # nolint end

  # change to svg
  app$set_inputs(`plot_with_settings-modal_downbutton-downl_state` = TRUE)
  app$set_inputs(`plot_with_settings-modal_downbutton-file_format` = "svg")
  app$set_inputs(
    `plot_with_settings-modal_downbutton-file_name` = "plot_svg_modal"
  )
  testthat::expect_equal(
    app$get_value(input = "plot_with_settings-modal_downbutton-file_name"),
    "plot_svg_modal"
  )

  # reset
  app$set_inputs(`plot_with_settings-modal_downbutton-downl_state` = FALSE)
})

# Testing hide and show button
testthat::test_that("plot_with_settings: hide/show button", {
  skip_on_cran()
  skip_on_ci()

  # nolint start
  # visible on load
  testthat::expect_true(
    app$get_js(
      "$('#plot_with_settings-plot-with-settings').is(':visible')"
    )
  )

  # hide
  app$click("button")
  testthat::expect_true(
    app$get_js(
      "$('#plot_with_settings-plot-with-settings').is(':hidden')"
    )
  )

  # unhide
  app$click("button")
  testthat::expect_true(
    app$get_js(
      "$('#plot_with_settings-plot-with-settings').is(':visible')"
    )
  )
  # nolint end
})

# tests width warning displays when width too low, hides when not.
# note that warning is not hidden/visible in the usual sense.
# rather, it has the fa icon <span> as a child or it does not.
# hence we're checking number of children.
testthat::test_that("plot_with_settings: width warning", {
  skip_on_cran()
  skip_on_ci()

  app$click("plot_with_settings-expbut")

  # starts out visible
  testthat::expect_equal(
    app$get_js("$('#plot_with_settings-width_warning').children().length"), 1
  )
  testthat::expect_equal(
    app$get_value(output = "plot_with_settings-width_warning")$html[1],
    paste0(
      "<span class=\"help-block\">\n  <i class=\"fa fa-triangle-exclamation\" role=\"presentation\" aria-label=",
      "\"triangle-exclamation icon\"></i>\n  Plot might be cut off for small widths.\n</span>"
    )
  )

  # now hidden
  app$set_inputs("plot_with_settings-width" = 600)
  # output can take a bit to update
  app$wait_for_value(
    output = "plot_with_settings-width_warning", ignore = list("")
  )
  testthat::expect_equal(
    app$get_js("$('#plot_with_settings-width_warning').children().length"), 0
  )

  # and back to visible
  app$set_inputs("plot_with_settings-width" = 300)
  app$wait_for_value(
    output = "plot_with_settings-width_warning", ignore = list("")
  )
  testthat::expect_equal(
    app$get_js("$('#plot_with_settings-width_warning').children().length"), 1
  )
})

# stop the app after finishing all tests
app$stop()

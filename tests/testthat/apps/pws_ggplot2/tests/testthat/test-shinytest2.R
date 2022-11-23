library(shinytest2)
library(testthat)

# one app opens for all tests
app <- AppDriver$new(name = "pws", height = 937, width = 1619)

# tests plot click functionalities
testthat::test_that("plot_with_settings: click functionalities ggplot2", {
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
  vals <- app$get_values()

  # check if outputs are reactive
  for (react_i in vals$export$"plot_data") {
    testthat::expect_true(is(react_i, "reactive"))
  }

  # check if output is desired plot
  testthat::expect_type(
    vals$output$`plot_with_settings-plot_main`$src, "character"
  )
  testthat::expect_true(
    grepl("data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAfQAAAGQCAIAAADX",
      vals$output$`plot_with_settings-plot_main`$src,
      fixed = TRUE
    )
  )
})

# download plots. expect_download() might not be stable, hence we test
# setting inputs and plot name changes
testthat::test_that("plot_with_settings: download functionality ggplot2", {
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

  testthat::expect_equal(
    shiny::isolate(vals$output$`plot_with_settings-plot_modal`$width), 750
  )
  testthat::expect_equal(
    shiny::isolate(vals$output$`plot_with_settings-plot_modal`$height), 1200
  )


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
})

# tests width warning displays when width too low, hides when not.
# note that warning is not hidden/visible in the usual sense.
# rather, it has the fa icon <span> as a child or it does not.
# hence we're checking number of children.
testthat::test_that("plot_with_settings: width warning", {
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

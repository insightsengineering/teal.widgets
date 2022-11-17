library(shinytest2)

app <- AppDriver$new(name = "pws_click", height = 937, width = 1619)

# tests plot clicking functionalities
test_that("{shinytest2} plot_with_settings: click functionalities ggplot2", {

  # brushing
  app$set_inputs(`plot_with_settings-plot_brush` = brush_vals, allow_no_input_binding_ = TRUE)
  vals <- app$get_values()
  test_brush <- isolate(vals$export$plot_data$brush())
  testthat::expect_equal(
    test_brush,
    brush_vals
  )
  app$set_inputs(`plot_with_settings-plot_brush` = character(0), allow_no_input_binding_ = TRUE)

  # hovering
  app$set_inputs(
    `plot_with_settings-plot_hover` = hover_vals, allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  vals <- app$get_values()
  test_hover <- isolate(vals$export$plot_data$hover())
  testthat::expect_equal(
    test_hover,
    hover_vals
  )

  # double click
  app$set_inputs(
    `plot_with_settings-plot_dblclick` = dbl_click_vals, allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  vals <- app$get_values()
  test_dblclick <- isolate(vals$export$plot_data$dblclick())
  testthat::expect_equal(
    test_dblclick,
    dbl_click_vals
  )

  # click
  app$set_inputs(
    `plot_with_settings-plot_click` = click_vals, allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  vals <- app$get_values()
  test_click <- isolate(vals$export$plot_data$click())
  testthat::expect_equal(
    test_click,
    click_vals
  )
})

test_that("{shinytest2} plot_with_settings: output is returned", {

  vals <- app$get_values()

  # check if outputs are reactive
  for (react_i in vals$export$'plot_data') {
    testthat::expect_true(is(react_i, "reactive"))
  }

  # check if plot is ggplot object
  testthat::expect_type(vals$output$`plot_with_settings-plot_main`$src, "character")
  testthat::expect_true(
    grepl("data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAfQAAAGQCAIAAADX",
          vals$output$`plot_with_settings-plot_main`$src,
          fixed = TRUE
          )
    )
})

test_that("{shinytest2} plot_with_settings: download functionality ggplot2", {


  # test default download options
  app$click("plot_with_settings-downbutton-downl")
  date <- strftime(Sys.time(), format = "%Y%m%d")
  testthat::expect_true(grepl(paste0("plot_", date), app$get_value(input = "plot_with_settings-downbutton-file_name")))
  testthat::expect_equal(app$get_value(input = "plot_with_settings-downbutton-file_format"), "png")

  # Download svg format
  app$set_inputs(`plot_with_settings-downbutton-downl_state` = TRUE)
  app$set_inputs(`plot_with_settings-downbutton-file_format` = "svg")
  app$set_inputs(`plot_with_settings-downbutton-file_name` = "plot_svg")
  app$click("plot_with_settings-downbutton-downl")

  testthat::expect_equal(app$get_value(input = "plot_with_settings-downbutton-file_name"), "plot_svg")

  app$set_inputs(`plot_with_settings-downbutton-downl_state` = FALSE)

})


test_that("{shinytest2} plot_with_settings: download ggplot2 modal", {

  # default downloading with modal
  app$set_inputs(`plot_with_settings-plot_hover` = character(0), allow_no_input_binding_ = TRUE)
  app$click("plot_with_settings-expand")
  app$click("plot_with_settings-modal_downbutton-downl")
  vals <- app$get_values()
  date <- strftime(Sys.time(), format = "%Y%m%d")
  testthat::expect_true(grepl(paste0("plot_", date), app$get_value(input = "plot_with_settings-modal_downbutton-file_name")))

  # Change width and height of plot on mondal
  app$click("plot_with_settings-expand")
  app$click("plot_with_settings-modal_downbutton-downl")
  app$set_inputs(`plot_with_settings-width_in_modal` = 2169)
  app$set_inputs(`plot_with_settings-height_in_modal` = 773)
  app$set_inputs(`plot_with_settings-modal_downbutton-downl_state` = TRUE)
  app$set_inputs(`plot_with_settings-modal_downbutton-file_name` = "plot_2")
  app$set_inputs(`plot_with_settings-modal_downbutton-file_format` = "pdf")

  vals <- app$get_values()
  testthat::expect_equal(app$get_value(input = "plot_with_settings-modal_downbutton-file_name"), "plot_2")
  testthat::expect_equal(isolate(vals$output$`plot_with_settings-plot_modal`$height), 773)
  testthat::expect_equal(isolate(vals$output$`plot_with_settings-plot_modal`$width), 2169)
  # change to svg
  app$set_inputs(`plot_with_settings-modal_downbutton-downl_state` = TRUE)
  app$set_inputs(`plot_with_settings-modal_downbutton-file_format` = "svg")
  app$set_inputs(`plot_with_settings-modal_downbutton-file_name` = "plot_svg_modal")
  testthat::expect_equal(app$get_value(input = "plot_with_settings-modal_downbutton-file_name"), "plot_svg_modal")

})

test_that("{shinytest2} plot_with_settings: hide/show button", {

  # visible on load
  expect_true(
    app$get_js(
      "$('#plot_with_settings-plot-with-settings').is(':visible')"
    )
  )

  # hide
  app$click("button")
  expect_true(
    app$get_js(
      "$('#plot_with_settings-plot-with-settings').is(':hidden')"
    )
  )

  # unhide
  app$click("button")
  expect_true(
    app$get_js(
      "$('#plot_with_settings-plot-with-settings').is(':visible')"
    )
  )

})

# tests width warning displays when width too low, hides when not.
# note that warning is not hidden/visible in the usual sense.
# rather, it has the fa icon <span> as a child or it does not.
# hence we're checking number of children.
test_that("{shinytest2} plot with settings: width warning", {

  app$click('plot_with_settings-expbut')

  # starts out visible
  expect_equal(
    app$get_js("$('#plot_with_settings-width_warning').children().length"),
    1
  )

  # now hidden
  app$set_inputs('plot_with_settings-width' = 600)
  # output can take a bit to update
  app$wait_for_value(output = 'plot_with_settings-width_warning', ignore = list(""))
  expect_equal(
    app$get_js("$('#plot_with_settings-width_warning').children().length"),
    0
  )

  # and back to visible
  app$set_inputs('plot_with_settings-width' = 300)
  app$wait_for_value(output = 'plot_with_settings-width_warning', ignore = list(""))
  expect_equal(
    app$get_js("$('#plot_with_settings-width_warning').children().length"),
    1
  )

})

app$stop()

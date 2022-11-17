library(shinytest2)

app <- AppDriver$new(name = "pws_click", height = 937, width = 1619)

# tests plot clicking functionalities
test_that("{shinytest2} plot with settings: click", {

  skip_on_cran()
  skip_on_ci()

  # brushing
  app$set_inputs(`plot_with_settings-plot_brush` = brush_vals(), allow_no_input_binding_ = TRUE)
  test_brush <- app$get_value(input = "plot_with_settings-plot_brush")
  expect_equal(
    test_brush,
    brush_vals()
  )
  app$set_inputs(`plot_with_settings-plot_brush` = character(0), allow_no_input_binding_ = TRUE)

  # hovering
  app$set_inputs(
    `plot_with_settings-plot_hover` = hover_vals(), allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  test_hover <- app$get_value(input = "plot_with_settings-plot_hover")
  expect_equal(
    test_hover,
    hover_vals()
  )

  # double click
  app$set_inputs(
    `plot_with_settings-plot_dblclick` = dbl_click_vals(), allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  test_dblclick <- app$get_value(input = "plot_with_settings-plot_dblclick")
  expect_equal(
    test_dblclick,
    dbl_click_vals()
  )

  # click
  app$set_inputs(
    `plot_with_settings-plot_click` = click_vals(), allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  test_click <- app$get_value(input = "plot_with_settings-plot_click")
  expect_equal(
    test_click,
    click_vals()
  )
})

test_that("{shinytest2} plot with settings: hide", {
  skip_on_cran()
  skip_on_ci()

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

  skip_on_cran()
  skip_on_ci()

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

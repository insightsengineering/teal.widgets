library(shinytest2)

# tests width warning displays when width too low, hides when not
# note that warning is not hidden/visible in the usual sense
# rather, it has the fa icon <span> as a child or it does not
# hence we're checking .children().length
test_that("{shinytest2} plot with settings: width warning", {

  skip_on_cran()
  skip_on_ci()

  app <- AppDriver$new(name = "width_warning", height = 937, width = 1619)

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

  app$stop()
})


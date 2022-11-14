library(shinytest2)

# tests plot clicking functionalities
test_that("{shinytest2} plot with settings: click trellis/lattice", {

  skip_on_cran()
  skip_on_ci()

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


  app <- AppDriver$new(name = "pws_click", height = 937, width = 1619)

  # brushing
  app$set_inputs(`plot_with_settings-plot_brush` = brush_vals, allow_no_input_binding_ = TRUE)
  test_brush <- app$get_value(input = "plot_with_settings-plot_brush")
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
  test_hover <- app$get_value(input = "plot_with_settings-plot_hover")
  testthat::expect_equal(
    test_hover,
    hover_vals
  )

  # double click
  app$set_inputs(
    `plot_with_settings-plot_dblclick` = dbl_click_vals, allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  test_dblclick <- app$get_value(input = "plot_with_settings-plot_dblclick")
  testthat::expect_equal(
    test_dblclick,
    dbl_click_vals
  )

  # click
  app$set_inputs(
    `plot_with_settings-plot_click` = click_vals, allow_no_input_binding_ = TRUE,
    priority_ = "event"
  )
  test_click <- app$get_value(input = "plot_with_settings-plot_click")
  testthat::expect_equal(
    test_click,
    click_vals
  )
})


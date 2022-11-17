library(shinytest2)

# tests plot clicking functionalities
test_that("{shinytest2} plot_with_settings: click functionalities ggplot2", {
  # skip_on_cran()
  # skip_on_ci()

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

test_that("{shinytest2} plot_with_settings: output is returned", {
  #skip_on_cran()
  #skip_on_ci()

  app <- AppDriver$new(name = "pws_download_ggplot2_types", height = 937, width = 1619)
  vals <- app$get_values()

  # check if outputs are reactive
  for (react_i in vals$export$'plot_data') {
    testthat::expect_true(is(react_i, "reactive"))
  }

  # check if plot is ggplot object
  testthat::expect_type(vals$output$`plot_with_settings-plot_main`$src, "character")
  testthat::expect_true(
    grepl("data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAABjUAAAGQCAIAAABp",
          vals$output$`plot_with_settings-plot_main`$src,
          fixed = TRUE
          )
    )
})

test_that("{shinytest2} plot_with_settings: download functionality ggplot2", {
  # skip_on_cran()
  # skip_on_ci()

  app <- AppDriver$new(name = "pws_download_ggplot2", height = 880, width = 1619)

  # test default download options
  app$click("plot_with_settings-downbutton-downl")
  #app$expect_download("plot_with_settings-downbutton-data_download")
  date <- strftime(Sys.time(), format = "%Y%m%d")
  testthat::expect_true(grepl(paste0("plot_", date), app$get_value(input = "plot_with_settings-downbutton-file_name")))
  testthat::expect_equal(app$get_value(input = "plot_with_settings-downbutton-file_format"), "png")

  # Download svg format
  app$set_inputs(`plot_with_settings-downbutton-downl_state` = TRUE)
  app$set_inputs(`plot_with_settings-downbutton-file_format` = "svg")
  app$set_inputs(`plot_with_settings-downbutton-file_name` = "plot_svg")
  app$click("plot_with_settings-downbutton-downl")

  vals <- app$get_values()
  testthat::expect_equal(app$get_value(input = "plot_with_settings-downbutton-file_name"), "plot_svg")

  app$set_inputs(`plot_with_settings-downbutton-downl_state` = FALSE)
  app$stop()
})

# test downloading with modal
test_that("{shinytest2} plot_with_settings: download ggplot2 modal", {
  # skip_on_cran()
  # skip_on_ci()
  app <- AppDriver$new(name = "pws_download_ggplot2_modal", height = 880, width = 1619)

  # default downloading with modal
  app$set_inputs(`plot_with_settings-plot_hover` = character(0), allow_no_input_binding_ = TRUE)
  app$click("plot_with_settings-expand")
  app$click("plot_with_settings-modal_downbutton-downl")
  vals <- app$get_values()
  date <- strftime(Sys.time(), format = "%Y%m%d")
  testthat::expect_true(grepl(paste0("plot_", date), app$get_value(input = "plot_with_settings-downbutton-file_name")))

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

  app$stop()
})

# test flex
test_that("{shinytest2} plot_with_settings: download ggplot2 modal", {
  # skip_on_cran()
  # skip_on_ci()
  app <- AppDriver$new(name = "pws_ggplot2_flex", height = 880, width = 1619)

  # default downloading with modal
  app$set_inputs(`plot_with_settings-plot_hover` = character(0), allow_no_input_binding_ = TRUE)
  app$click("plot_with_settings-expand")
  app$click("plot_with_settings-modal_downbutton-downl")
  vals <- app$get_values()
  date <- strftime(Sys.time(), format = "%Y%m%d")
  testthat::expect_true(grepl(paste0("plot_", date), app$get_value(input = "plot_with_settings-downbutton-file_name")))

  # Change width and height of plot on mondal
  app$click("plot_with_settings-expand")
  app$set_inputs(`plot_with_settings-width_in_modal` = 2169)
  app$set_inputs(`plot_with_settings-height_in_modal` = 773)
  app$set_inputs(`plot_with_settings-modal_downbutton-downl_state` = TRUE)
  app$set_inputs(`plot_with_settings-modal_downbutton-file_name` = "plot_2")
  app$set_inputs(`plot_with_settings-modal_downbutton-file_format` = "pdf")
  app$click("plot_with_settings-modal_downbutton-downl")

  vals <- app$get_values()
  testthat::expect_equal(app$get_value(input = "plot_with_settings-modal_downbutton-file_name"), "plot_2")
  testthat::expect_equal(isolate(vals$output$`plot_with_settings-plot_modal`$height), 773)
  testthat::expect_equal(isolate(vals$output$`plot_with_settings-plot_modal`$width), 2169)

  # change to svg
  app$set_inputs(`plot_with_settings-modal_downbutton-downl_state` = TRUE)
  app$set_inputs(`plot_with_settings-modal_downbutton-file_format` = "svg")
  app$set_inputs(`plot_with_settings-modal_downbutton-file_name` = "plot_svg_modal")
  testthat::expect_equal(app$get_value(input = "plot_with_settings-modal_downbutton-file_name"), "plot_svg_modal")

  app$stop()
})

test_that("{shinytest2} plot with settings: download ggplot2 svg", {
  # skip_on_cran()
  # skip_on_ci()

  input_vals <- c("0", "1", "TRUE", "svg",  "plot_svg", "0", "0", "1589", "1408.53")

  app <- AppDriver$new(name = "pws_download_ggplot2_svg", height = 880, width = 1619)
  app$set_inputs(`plot_with_settings-plot_hover` = character(0), allow_no_input_binding_ = TRUE)
  app$click("plot_with_settings-downbutton-downl")
  app$set_inputs(`plot_with_settings-downbutton-downl_state` = TRUE)
  app$set_inputs(`plot_with_settings-downbutton-file_format` = "svg")
  app$set_inputs(`plot_with_settings-downbutton-file_name` = "plot_svg")
  vals <- app$get_values()
  testthat::expect_equal(as.vector(unlist(vals$input)), input_vals)

  app$stop()
})

test_that("{shinytest2} plot_with_settings: hide/show button", {
  # skip_on_cran()
  # skip_on_ci()
  app <- AppDriver$new(name = "pwd_show_hide", height = 937, width = 1619)

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

library(shinytest2)

# TODO:
# exportTestValues for this file
# exportTestValues for pws_click
# refactor table with settings tests + exportTestValues
# almost all above for GRID, lattice
# test warning for setting width too low

test_that("{shinytest2} plot with settings: output types", {
  skip_on_cran()
  skip_on_ci()
  app <- AppDriver$new(name = "pws_download_ggplot2_types", height = 937, width = 1619)
  vals <- app$get_values()

  # check if outputs are reactive
  testthat::expect_true(is(vals$export$'plot_r', "reactiveExpr"))
  testthat::expect_true(is(vals$export$'plot_r', "reactive"))
  testthat::expect_true(is(vals$export$'plot_r', "function"))
  for (react_i in vals$export$'plot_data') {
    testthat::expect_true(is(react_i, "reactive"))
  }

  # check if plot is ggplot object
  testthat::expect_true(is(isolate(rlang::inject(vals$export$'plot_r')()), "ggplot"))
  testthat::expect_true(is.character(vals$output$`plot_with_settings-plot_main`$src))
  testthat::expect_true(grepl("data\\:image\\/png\\;base64\\,", vals$output$`plot_with_settings-plot_main`$src))
  # testthat::expect_equal(nchar(vals$output$`plot_with_settings-plot_main`$src), 8578)
})


test_that("{shinytest2} plot with settings: download ggplot2", {
  skip_on_cran()
  skip_on_ci()

  app <- AppDriver$new(name = "pws_download_ggplot2", height = 937, width = 1619)
  app$click("plot_with_settings-downbutton-downl")
  app$set_inputs(`plot_with_settings-downbutton-downl_state` = TRUE)
  app$set_inputs(`plot_with_settings-downbutton-file_name` = "plot_1")
  app$expect_download("plot_with_settings-downbutton-data_download")

  testthat::expect_equal(
    app$get_value(input = "plot_with_settings-downbutton-file_name"), "plot_1"
    )

  app$set_inputs(`plot_with_settings-downbutton-downl_state` = FALSE)

  app$set_window_size(width = 1619, height = 880)
  app$click("plot_with_settings-expand")
  app$set_inputs(`plot_with_settings-width_in_modal` = 2169)
  app$set_inputs(`plot_with_settings-height_in_modal` = 773)

  app$click("plot_with_settings-modal_downbutton-downl")
  app$set_inputs(`plot_with_settings-modal_downbutton-downl_state` = TRUE)
  app$set_inputs(`plot_with_settings-modal_downbutton-file_name` = "plot_2")
  plot_name2 <- app$get_value(input = "plot_with_settings-modal_downbutton-file_name")
  testthat::expect_equal(plot_name2, "plot_2")


  app$stop()
})


test_that("{shinytest2} plot with settings: download ggplot2 svg modal", {
  skip_on_cran()
  skip_on_ci()

  app <- AppDriver$new(name = "pws_download_ggplot2_svg_modal", height = 880, width = 1619)
  app$set_inputs(`plot_with_settings-plot_hover` = character(0), allow_no_input_binding_ = TRUE)
  app$click("plot_with_settings-expand")
  app$click("plot_with_settings-modal_downbutton-downl")
  app$set_inputs(`plot_with_settings-height_in_modal` = 400)
  app$set_inputs(`plot_with_settings-width_in_modal` = 1409)
  app$set_inputs(`plot_with_settings-modal_downbutton-file_format` = "png")
  app$set_inputs(`plot_with_settings-modal_downbutton-file_name` = "plot_x")
  app$set_inputs(`plot_with_settings-width_in_modal` = 1921)
  app$set_inputs(`plot_with_settings-height_in_modal` = 728)
  app$click("plot_with_settings-modal_downbutton-downl")
  app$set_inputs(`plot_with_settings-modal_downbutton-downl_state` = TRUE)
  app$set_inputs(`plot_with_settings-modal_downbutton-file_format` = "svg")
  app$set_inputs(`plot_with_settings-modal_downbutton-file_name` = "plot_svg_modal")
  testthat::expect_equal(
    app$get_value(input = "plot_with_settings-modal_downbutton-file_name"), "plot_svg_modal"
  )
  app$stop()
})


test_that("{shinytest2} plot with settings: download ggplot2 svg", {
  skip_on_cran()
  skip_on_ci()

  input_vals <- c("1", "TRUE", "svg",  "plot_svg", "0", "0", "1589", "1408.53")

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

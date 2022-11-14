library(shinytest2)

# download as .png
test_that("{shinytest2} recording: pws_dwnl", {
  skip_on_cran()
  skip_on_ci()

  app <- AppDriver$new(name = "pws_dwnl", height = 937, width = 1619)

  # download plot as initialized
  # saving images or svg (below) might be brittle
  # i wonder if the 'compare' argument of AppDriver$expect_download can
  #  help here
  # another option might be to use shiny::exportTestValues() within the module
  #  server
  # then we could check information about the plot (class, input data, ...)
  # in that case, maybe an expect_error(app$get_download('..'), NA) could
  #  suffice for download functionality
  app$click("plot_with_settings-downbutton-downl")
  app$set_inputs(`plot_with_settings-downbutton-downl_state` = TRUE)
  app$set_inputs(`plot_with_settings-downbutton-file_name` = "plot_1")
  app$expect_download("plot_with_settings-downbutton-data_download")

  plot_name1 <- app$get_value(input = "plot_with_settings-downbutton-file_name")
  testthat::expect_equal(plot_name1, "plot_1")

  app$set_inputs(`plot_with_settings-downbutton-downl_state` = FALSE)

  # why the change in window size?
  app$set_window_size(width = 1619, height = 880)
  app$click("plot_with_settings-expand")
  # if we're going to change the values, i would check they update correctly
  # e.g. height <- app$get_values(input = 'plot_with_settings...)
  #      expect_equal(heigth, 400)
  app$set_inputs(`plot_with_settings-height_in_modal` = 400)
  app$set_inputs(`plot_with_settings-width_in_modal` = 1409)
  app$set_inputs(`plot_with_settings-width_in_modal` = 2169)
  app$set_inputs(`plot_with_settings-height_in_modal` = 773)

  app$click("plot_with_settings-modal_downbutton-downl")
  app$set_inputs(`plot_with_settings-modal_downbutton-downl_state` = TRUE)
  app$set_inputs(`plot_with_settings-modal_downbutton-file_name` = "plot_2")
  app$expect_download("plot_with_settings-modal_downbutton-data_download")
  plot_name2 <- app$get_value(input = "plot_with_settings-modal_downbutton-file_name")
  testthat::expect_equal(plot_name2, "plot_2")
  app$stop()
})


test_that("{shinytest2} recording: pws_svg_modal", {
  skip_on_cran()
  skip_on_ci()

  app <- AppDriver$new(name = "pws_svg_modal", height = 880, width = 1619)
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
  app$set_inputs(`plot_with_settings-modal_downbutton-file_name` = "plot_svg")
  app$expect_download("plot_with_settings-modal_downbutton-data_download")

  app$stop()
})


test_that("{shinytest2} recording: pws_svg", {
  skip_on_cran()
  skip_on_ci()

  app <- AppDriver$new(name = "pws_svg", height = 880, width = 1619)
  app$set_inputs(`plot_with_settings-plot_hover` = character(0), allow_no_input_binding_ = TRUE)
  app$click("plot_with_settings-downbutton-downl")
  app$set_inputs(`plot_with_settings-downbutton-downl_state` = TRUE)
  app$set_inputs(`plot_with_settings-downbutton-file_format` = "svg")
  app$set_inputs(`plot_with_settings-downbutton-file_name` = "plot_svg")
  app$expect_download("plot_with_settings-downbutton-data_download")

  app$stop()
})

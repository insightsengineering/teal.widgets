library(shinytest2)

test_that("{shinytest2} plot with settings: hide", {
  skip_on_cran()
  skip_on_ci()

  input_vals <- c("0", "1", "TRUE", "svg", "plot_z", "0", "1", "FALSE", "1318", "808", "1187.55", "568", "FALSE")

  app <- AppDriver$new(name = "pws_hide", height = 703, width = 1365)
  app$click("plot_with_settings-expbut")
  app$set_inputs(`plot_with_settings-expbut_state` = TRUE)
  app$set_inputs(`plot_with_settings-width` = 410)
  app$set_inputs(`plot_with_settings-height` = 808)
  app$set_inputs(`plot_with_settings-flex_width` = 1318, allow_no_input_binding_ = TRUE)
  app$set_inputs(`plot_with_settings-width` = 568)
  app$click("plot_with_settings-downbutton-downl")
  app$set_inputs(`plot_with_settings-expbut_state` = FALSE)
  app$set_inputs(`plot_with_settings-downbutton-downl_state` = TRUE)
  app$set_inputs(`plot_with_settings-downbutton-file_format` = "svg")
  app$set_inputs(`plot_with_settings-downbutton-file_name` = "plot_z")

  vals <- app$get_values()

  testthat::expect_equal(as.vector(unlist(vals$input)), input_vals)
})


library(shinytest2)

test_that("{shinytest2} plot with settings: grid output types", {
  skip_on_cran()
  skip_on_ci()

  input_vals = c("0", "png", "plot_xyz", "0", "0", "1589", "1408.53")

  app <- AppDriver$new(name = "pws_download_grid_types", height = 937, width = 1619)
  app$set_inputs(`plot_with_settings-downbutton-file_name` = "plot_xyz")
  vals <- app$get_values()

  # outputs are reactive
  testthat::expect_true(is(vals$export$'plot_r', "reactiveExpr"))
  testthat::expect_true(is(vals$export$'plot_r', "reactive"))
  for (react_i in vals$export$'plot_data') {
    testthat::expect_true(is(react_i, "reactive"))
  }

  # plot exists
  testthat::expect_true(is.character(vals$output$`plot_with_settings-plot_main`$src))
  testthat::expect_true(grepl("data\\:image\\/png\\;base64\\,", vals$output$`plot_with_settings-plot_main`$src))

  # input values
  testthat::expect_equal(as.vector(unlist(vals$input)), input_vals)
})

library(shinytest2)


test_that("{shinytest2} recording: pws_click", {
  app <- AppDriver$new(name = "pws_click", height = 937, width = 1619)
  app$set_inputs(`plot_with_settings-plot_brush` = c(1.575922029932, 2.2959573557845,
      1.218473025872, 2.2972720836904, 252, 532, 251, 349, 252, 532, 251, 349, 1,
      1, "1:5", "1:5", 1, 5, 1, 5, 28.0413099315069, 1583.52054794521, 368.846473150198,
      5.47945205479452, character(0), character(0), "xy", "plot_with_settings-plot_brush",
      "plot_with_settings-plot_main"), allow_no_input_binding_ = TRUE)
  test1 <- app$get_value(input = "plot_with_settings-plot_brush")
  testthat::expect_equal(
    test1,
    c(1.575922029932, 2.2959573557845,
      1.218473025872, 2.2972720836904, 252, 532, 251, 349, 252, 532, 251, 349, 1,
      1, "1:5", "1:5", 1, 5, 1, 5, 28.0413099315069, 1583.52054794521, 368.846473150198,
      5.47945205479452, character(0), character(0), "xy", "plot_with_settings-plot_brush",
      "plot_with_settings-plot_main"))
  app$set_inputs(`plot_with_settings-plot_hover` = c(2.95684692272772, 2.34130469829519,
      789, 247, 789, 247, 1, 1, "1:5", "1:5", 1, 5, 1, 5, 28.0413099315069, 1583.52054794521,
      368.846473150198, 5.47945205479452, character(0), character(0)), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  test2 <- app$get_value(input = "plot_with_settings-plot_hover")
  testthat::expect_equal(
    test2,
    c(2.95684692272772, 2.34130469829519, 789, 247, 789, 247, 1, 1,
      "1:5", "1:5", 1, 5, 1, 5, 28.0413099315069, 1583.52054794521,
      368.846473150198, 5.47945205479452, character(0), character(0)))
  app$set_inputs(`plot_with_settings-plot_brush` = character(0), allow_no_input_binding_ = TRUE)
  app$set_inputs(`plot_with_settings-plot_dblclick` = c(2.95684692272772, 2.34130469829519,
      789, 247, 789, 247, 1, 1, "1:5", "1:5", 1, 5, 1, 5, 28.0413099315069, 1583.52054794521,
      368.846473150198, 5.47945205479452, character(0), character(0)), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  test3 <- app$get_value(input = "plot_with_settings-plot_dblclick")
  testthat::expect_equal(
    test3,
    c(2.95684692272772, 2.34130469829519, 789, 247, 789, 247, 1, 1,
      "1:5", "1:5", 1, 5, 1, 5, 28.0413099315069, 1583.52054794521,
      368.846473150198, 5.47945205479452, character(0), character(0)))
  app$set_inputs(`plot_with_settings-plot_click` = c(2.95941847746291, 1.9450111668517,
      790, 283, 790, 283, 1, 1, "1:5", "1:5", 1, 5, 1, 5, 28.0413099315069, 1583.52054794521,
      368.846473150198, 5.47945205479452, character(0), character(0)), allow_no_input_binding_ = TRUE,
      priority_ = "event")
    test4 <- app$get_value(input = "plot_with_settings-plot_click")
    testthat::expect_equal(
      test4,
      c(2.95941847746291, 1.9450111668517, 790, 283, 790, 283, 1, 1,
        "1:5", "1:5", 1, 5, 1, 5, 28.0413099315069, 1583.52054794521,
        368.846473150198, 5.47945205479452, character(0), character(0)))
})


test_that("{shinytest2} recording: pws_dwnl", {
  app <- AppDriver$new(name = "pws_dwnl", height = 937, width = 1619)
  app$set_inputs(`plot_with_settings-plot_hover` = character(0), allow_no_input_binding_ = TRUE)
  app$click("plot_with_settings-downbutton-downl")
  app$set_inputs(`plot_with_settings-downbutton-downl_state` = TRUE)
  app$set_inputs(`plot_with_settings-downbutton-file_name` = "plot_1")
  app$expect_download("plot_with_settings-downbutton-data_download")
  plot_name1 <- app$get_value(input = "plot_with_settings-downbutton-file_name")
  testthat::expect_equal(plot_name1, "plot_1")
  app$set_window_size(width = 1619, height = 880)
  app$click("plot_with_settings-expand")
  app$set_inputs(`plot_with_settings-downbutton-downl_state` = FALSE)
  app$click("plot_with_settings-modal_downbutton-downl")
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


test_that("{shinytest2} recording: pws_svg", {
  app <- AppDriver$new(name = "pws_svg", height = 880, width = 1619)
  app$set_inputs(`plot_with_settings-plot_hover` = character(0), allow_no_input_binding_ = TRUE)
  app$click("plot_with_settings-downbutton-downl")
  app$set_inputs(`plot_with_settings-downbutton-downl_state` = TRUE)
  app$set_inputs(`plot_with_settings-downbutton-file_format` = "svg")
  app$set_inputs(`plot_with_settings-downbutton-file_name` = "plot_svg")
  app$expect_download("plot_with_settings-downbutton-data_download")
})


test_that("{shinytest2} recording: pws_svg_modal", {
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
})

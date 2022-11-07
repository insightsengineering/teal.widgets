library(shinytest2)

test_that("{shinytest2} recording: pws1", {
  app <- AppDriver$new(name = "pws1", height = 849, width = 1473)
  app$set_inputs(`plot_with_settings-plot_hover` = c(4.06484454951012, 2.31928839099277,
      1108, 249, 1108, 249, 1, 1, "1:5", "1:5", 1, 5, 1, 5, 28.0413099315069, 1437.52054794521,
      368.846473150198, 5.47945205479452, character(0), character(0)), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plot_with_settings-plot_hover` = character(0), allow_no_input_binding_ = TRUE)
  app$set_inputs(`plot_with_settings-plot_hover` = c(4.044979056465, 3.0238102246701,
      1101, 185, 1101, 185, 1, 1, "1:5", "1:5", 1, 5, 1, 5, 28.0413099315069, 1437.52054794521,
      368.846473150198, 5.47945205479452, character(0), character(0)), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plot_with_settings-plot_hover` = c(2.75088408095443, 3.64026682913775,
      645, 129, 645, 129, 1, 1, "1:5", "1:5", 1, 5, 1, 5, 28.0413099315069, 1437.52054794521,
      368.846473150198, 5.47945205479452, character(0), character(0)), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plot_with_settings-plot_brush` = character(0), allow_no_input_binding_ = TRUE)
  app$set_inputs(`plot_with_settings-plot_click` = c(2.75088408095443, 3.64026682913775,
      645, 129, 645, 129, 1, 1, "1:5", "1:5", 1, 5, 1, 5, 28.0413099315069, 1437.52054794521,
      368.846473150198, 5.47945205479452, character(0), character(0)), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plot_with_settings-plot_click` = c(2.39614313372017, 4.13563374344212,
      520, 84, 520, 84, 1, 1, "1:5", "1:5", 1, 5, 1, 5, 28.0413099315069, 1437.52054794521,
      368.846473150198, 5.47945205479452, character(0), character(0)), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plot_with_settings-plot_brush` = c(2.3961431337202, 3.3156316689514,
      1.5267013281058, 4.1356337434421, 520, 844, 84, 321, 520, 844, 84, 321, 1,
      1, "1:5", "1:5", 1, 5, 1, 5, 28.0413099315069, 1437.52054794521, 368.846473150198,
      5.47945205479452, character(0), character(0), "xy", "plot_with_settings-plot_brush",
      "plot_with_settings-plot_main"), allow_no_input_binding_ = TRUE)
  app$set_inputs(`plot_with_settings-plot_hover` = c(3.31563166895137, 1.52670132810579,
      844, 321, 844, 321, 1, 1, "1:5", "1:5", 1, 5, 1, 5, 28.0413099315069, 1437.52054794521,
      368.846473150198, 5.47945205479452, character(0), character(0)), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plot_with_settings-plot_brush` = character(0), allow_no_input_binding_ = TRUE)
  app$set_inputs(`plot_with_settings-plot_hover` = c(4.58986115141682, 2.71558192243627,
      1293, 213, 1293, 213, 1, 1, "1:5", "1:5", 1, 5, 1, 5, 28.0413099315069, 1437.52054794521,
      368.846473150198, 5.47945205479452, character(0), character(0)), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`plot_with_settings-plot_click` = c(4, 2,
      1293, 213, 1293, 213, 1, 1, "1:5", "1:5", 1, 5, 1, 5, 28, 1437,
      368, 5, character(0), character(0)), allow_no_input_binding_ = TRUE,
      priority_ = "event")
  w1 <- app$get_value(input = "plot_with_settings-plot_click")
  testthat::expect_equal(w1[1], "4")
})

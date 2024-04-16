testthat::test_that("e2e - tm_a_regerssion: clicking plot settings accordion panel shows plot setting", {
  skip_if_too_deep(5)

  app <- app_driver_tm_a_regression()
  app$expect_no_shiny_error()

  testthat::expect_false(app$is_visible(app$active_module_element("size")))
  testthat::expect_false(app$is_visible(app$active_module_element("alpha")))
  testthat::expect_false(app$is_visible(app$active_module_element("label_min_segment")))
  testthat::expect_false(app$is_visible(app$active_module_element("module-ggtheme")))

  # After click they are visible.
  app$click(selector = "#_div > div.panel-heading.collapsed")
  testthat::expect_true(app$is_visible(app$active_module_element("size")))
  testthat::expect_true(app$is_visible(app$active_module_element("alpha")))
  testthat::expect_true(app$is_visible(app$active_module_element("label_min_segment")))
  testthat::expect_true(app$is_visible(app$active_module_element("ggtheme")))

  app$stop()
})

testthat::test_that("e2e - tm_a_regerssion: plot settings have default values", {
  skip_if_too_deep(5)

  app <- app_driver_tm_a_regression()
  app$expect_no_shiny_error()

  app$click(selector = "#_div > div.panel-heading.collapsed")

  testthat::expect_identical(app$get_active_module_input("size"), 2)
  testthat::expect_identical(app$get_active_module_input("alpha"), 1)
  testthat::expect_identical(app$get_active_module_input("label_min_segment"), 0.5)
  testthat::expect_identical(app$get_active_module_input("ggtheme"), "gray")

  app$set_module_input("size", 3)
  app$set_module_input("alpha", 0.5)
  app$set_module_input("label_min_segment", 0.6)
  app$set_module_input("ggtheme", "bw")
  app$expect_no_validation_error()

  app$stop()
})

test_that("panel_group throws deprecation warning", {
  lifecycle::expect_deprecated(panel_group())
})

test_that("panel_group returns a bslib::accordion object", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_s3_class(panel_group(), "shiny.tag")
  expect_s3_class(panel_group(), "bslib_fragment")
})

test_that("panel_item throws deprecation warning", {
  lifecycle::expect_deprecated(panel_item(title = "a title"))
})

test_that("panel_item returns a bslib::accordion object", {
  withr::local_options(lifecycle_verbosity = "quiet")
  item <- panel_item(title = "a title")
  expect_s3_class(item, "shiny.tag")
  testthat::expect_equal(item$attribs$class, "accordion-item")
})

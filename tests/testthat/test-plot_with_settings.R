library(shinytest2)

testthat::test_that("if teal.plot_dpi is not set then get_plot_dpi returns 72 ", {
  withr::with_options(
    list(teal.plot_dpi = NULL),
    testthat::expect_equal(get_plot_dpi(), 72)
  )
})

testthat::test_that("if teal.plot_dpi is an integer value at least 24 then get_plot_dpi returns its value", {
  withr::with_options(
    list(teal.plot_dpi = 24),
    testthat::expect_equal(get_plot_dpi(), 24)
  )
  withr::with_options(
    list(teal.plot_dpi = 48),
    testthat::expect_equal(get_plot_dpi(), 48)
  )
  withr::with_options(
    list(teal.plot_dpi = 72),
    testthat::expect_equal(get_plot_dpi(), 72)
  )
  withr::with_options(
    list(teal.plot_dpi = 96),
    testthat::expect_equal(get_plot_dpi(), 96)
  )
})

testthat::test_that("if teal.plot_dpi is an integer value less 24 then get_plot_dpi returns 72", {
  testthat::expect_warning(
    withr::with_options(
      list(teal.plot_dpi = 23),
      testthat::expect_equal(get_plot_dpi(), 72)
    ),
    "teal.plot_dpi"
  )
  testthat::expect_warning(
    withr::with_options(
      list(teal.plot_dpi = 0),
      testthat::expect_equal(get_plot_dpi(), 72)
    ),
    "teal.plot_dpi"
  )
})

testthat::test_that("if teal.plot_dpi is not an integer value then get_plot_dpi returns 72", {
  testthat::expect_warning(
    withr::with_options(
      list(teal.plot_dpi = c(72, 96)),
      testthat::expect_equal(get_plot_dpi(), 72)
    ),
    "teal.plot_dpi"
  )

  testthat::expect_warning(
    withr::with_options(
      list(teal.plot_dpi = "foo"),
      testthat::expect_equal(get_plot_dpi(), 72)
    ),
    "teal.plot_dpi"
  )
})


test_that("table-with-settings", {
  skip_on_cran()

  appdir <- system.file(package = "teal.widgets", "pws1")
  shinytest2::test_app(appdir)
})


testthat::test_that("type_download_ui returns shiny.tag", {
  testthat::expect_s3_class(type_download_ui("STH"), "shiny.tag")
})

testthat::test_that("plot_with_settings_ui returns shiny.tag.list", {
  testthat::expect_s3_class(plot_with_settings_ui("STH"), "shiny.tag.list")
})

library(shinytest2)

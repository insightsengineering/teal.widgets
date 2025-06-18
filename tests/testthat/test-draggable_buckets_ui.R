app_driver_db <- function() {
  ui <- bslib::page_fluid(
    draggable_buckets("id", "Choices #1", c("a", "b"), c("bucket1", "bucket2"))
  )

  shiny::shinyApp(ui, function(input, output) {})
}

testthat::test_that(
  "e2e: teal.widgets::draggable_buckets: initializes",
  {
    skip_if_too_deep(5)
    app_driver <- shinytest2::AppDriver$new(
      app_driver_db(),
      name = "db",
      variant = "app_driver_db_ui"
    )
    app_driver$wait_for_idle(timeout = default_idle_timeout)
    testthat::expect_true(is_visible("body > div", app_driver))
    testthat::expect_true(is_visible(".draggableBuckets.shiny-bound-input", app_driver))
    app_driver$stop()
  }
)

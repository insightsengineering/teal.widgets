app_driver_osivmm <- function() {
  ui <- bslib::page_fluid(
    shinyjs::useShinyjs(),
    tags$div(
      optionalSliderInputValMinMax("a1", "b1", 1)
    ),
    optionalSliderInputValMinMax("a2", "b2", c(3, 1, 5))
  )
  shiny::shinyApp(ui, function(input, output) {})
}


testthat::test_that(
  "e2e: teal.widgets::optionalSliderInputValMinMax: initializes",
  {
    skip_if_too_deep(5)
    app_driver <- shinytest2::AppDriver$new(
      app_driver_osivmm(),
      name = "osivmm",
      variant = "app_driver_osivmm_ui"
    )
    app_driver$wait_for_idle(timeout = default_idle_timeout)
    testthat::expect_false(is_visible("#a1-label", app_driver))
    testthat::expect_true(is_visible("#a2-label", app_driver))
    app_driver$stop()
  }
)

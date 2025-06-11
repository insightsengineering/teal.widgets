
app_driver_ncm <- function() {
  ui <- bslib::page_fluid(
    shinyjs::useShinyjs(),
    actionButton("show_1", "$(\"#modal_1\").modal(\"show\")"),
    nested_closeable_modal(
      "modal_1",
      modal_args = list(
        size = "l",
        title = "One Modal",
        easyClose = TRUE,
        footer = NULL
      ),
      tags$div("one modal")
    )
  )
  shiny::shinyApp(ui, function(input, output) {
    observeEvent(input$show_1, {
      shinyjs::runjs("$(\"#modal_1\").modal(\"show\")")
    })
  })
}
click_modal <- "document.querySelector('#show_1').click();"

testthat::test_that(
  "e2e: teal.widgets::nested_closeable_modal: initializes",
  {
    skip_if_too_deep(5)
    testthat::skip_if_not_installed("shinyjs")
    app_driver <- shinytest2::AppDriver$new(
      app_driver_ncm(),
      name = "ncm",
      variant = "app_driver_ncm_ui"
    )
    app_driver$wait_for_idle(timeout = default_idle_timeout)
    testthat::expect_true(is_visible("#show_1", app_driver))
    testthat::expect_false(is_visible(".modal-content", app_driver))
    app_driver$stop()
  })

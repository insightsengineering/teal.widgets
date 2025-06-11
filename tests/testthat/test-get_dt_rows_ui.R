#' Plot with settings app
#'
#' @description Example plot with setting app for testing using \code{shinytest2}
#'
#' @keywords internal
#'
app_driver_gdr <- function() {
  ui <- function(id) {
    ns <- NS(id)
    tagList(
      DT::DTOutput(ns("data_table")),
      get_dt_rows(ns("data_table"), ns("dt_rows"))
    )
  }

  # use the input$dt_rows in the Shiny Server function
  server <- function(id) {
    moduleServer(id, function(input, output, session) {
      output$data_table <- renderDataTable(
        {
          iris
        },
        options = list(pageLength = input$dt_rows)
      )
    })
  }

  shinyApp(
    ui = ui("my_table_module"),
    server = function(input, output, session) server("my_table_module")
  )
}


testthat::test_that(
  "e2e: teal.widgets::get_dt_rows: table is visible",
  {
    skip_if_too_deep(5)
    testthat::skip_if_not_installed("DT")
    app_driver <- shinytest2::AppDriver$new(
      app_driver_gdr(),
      name = "gdr",
      variant = "app_driver_gdr_ui"
    )
    app_driver$wait_for_idle(timeout = default_idle_timeout)
    testthat::expect_true(is_visible("#my_table_module-data_table", app_driver))
    app_driver$view()
  }
)

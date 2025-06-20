app_driver_gdr <- function() {
  testthat::skip_if_not_installed("DT")
  ui <- function(id) {
    ns <- NS(id)
    tagList(
      get_dt_rows(ns("data_table"), ns("dt_rows")),
      textOutput(ns("rows")),
      DT::DTOutput(ns("data_table"))
    )
  }

  # use the input$dt_rows in the Shiny Server function
  server <- function(id) {
    moduleServer(id, function(input, output, session) {
      output$data_table <- DT::renderDataTable(iris)
      rows <- reactive({
        paste0("Selected Rows ", input$dt_rows)
      })
      output$rows <- renderText(rows())
    })
  }

  shinyApp(
    ui = ui("my_table_module"),
    server = function(input, output, session) server("my_table_module")
  )
}

testthat::test_that(
  "e2e: teal.widgets::get_dt_rows: rows are settable and visible",
  {
    skip_if_too_deep(5)
    app_driver <- shinytest2::AppDriver$new(
      app_driver_gdr(),
      name = "gdr",
      variant = "app_driver_gdr_ui"
    )
    app_driver$wait_for_idle(timeout = default_idle_timeout)
    testthat::expect_true(is_visible("#my_table_module-data_table", app_driver))

    # Check initial value
    dt_state <- app_driver$get_values(input = "my_table_module-data_table_state")
    testthat::expect_equal(dt_state$input$`my_table_module-data_table_state`$length, 10)

    # Change rows value
    # FIXME: Couldn't find a way to programatically change that

    # Check value again
    dt_state <- app_driver$get_values(input = "my_table_module-data_table_state")
    testthat::expect_equal(dt_state$input$`my_table_module-data_table_state`$length, 10)

    app_driver$stop()
  }
)

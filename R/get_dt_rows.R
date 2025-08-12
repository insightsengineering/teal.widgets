#' Map `lenghtMenu` property
#'
#' @description
#' Maps the `lengthMenu` selected value property of `DT::datatable` to a `shiny` variable.
#' @param dt_name `ns()` of `inputId` of the `DT::datatable`
#' @param dt_rows `ns()` of `inputId` of the variable that holds the current selected value of `lengthMenu`
#'
#' @name get_dt_rows
#'
#' @return (`shiny::tagList`) A `shiny tagList`.
#'
#' @examplesIf require("DT")
#' library(shiny)
#' library(DT)
#'
#' ui <- function(id) {
#'   ns <- NS(id)
#'   tagList(
#'     get_dt_rows(ns("data_table"), ns("dt_rows")),
#'     textOutput(ns("rows")),
#'     DT::DTOutput(ns("data_table"))
#'   )
#' }
#'
#' # use the input$dt_rows in the Shiny Server function
#' server <- function(id) {
#'   moduleServer(id, function(input, output, session) {
#'     output$data_table <- DT::renderDataTable(iris)
#'     # Change rows selected to see the first line on the UI change
#'     rows <- reactive({
#'       paste0("Selected Rows ", input$dt_rows)
#'     })
#'     output$rows <- renderText(rows())
#'   })
#' }
#' if (interactive()) {
#'   shinyApp(
#'     ui = ui("my_table_module"),
#'     server = function(input, output, session) server("my_table_module")
#'   )
#' }
#' @export
get_dt_rows <- function(dt_name, dt_rows) {
  tags$head(
    tags$script(
      sprintf(
        "$(document).ready(function() {
        $('%s').on('length.dt', function(e, settings, len) {
        Shiny.setInputValue('%s', len);
        });
        });",
        paste0("#", dt_name),
        dt_rows
      )
    )
  )
}

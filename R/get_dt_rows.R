#' Maps the `lengthMenu`selected value property of `DT::datatable` to a Shiny variable.
#'
#' @description `r lifecycle::badge("stable")`
#' @param dt_name \code{ns()} of `inputId` of the `DT::datatable`
#' @param dt_rows \code{ns()} of `inputId` of the variable that holds the current selected value of `lengthMenu`
#'
#' @name get_dt_rows
#'
#' @examples
#' \dontrun{
#' library(shiny)
#' # in Shiny UI function
#' tagList(
#'   get_dt_rows(ns("data_table"), ns("dt_rows")),
#'   ...
#' )
#'
#' # use the input$dt_rows in the Shiny Server function
#' if (!is.null(input$dt_rows)) {
#'   dt_args$options$pageLength <- input$dt_rows
#' } # nolint
#' do.call(DT::datatable, dt_args)
#' }
#'
#' @export
get_dt_rows <- function(dt_name, dt_rows) {
  tags$head(
    tags$script(
      sprintf(
        "$(document).ready(function() {
        $('%s').on('length.dt', function(e, settings, len) {
        Shiny.onInputChange('%s', len);
        });
        });",
        paste0("#", dt_name),
        dt_rows
      )
    )
  )
}

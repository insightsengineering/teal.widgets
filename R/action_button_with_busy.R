#' Teal action button that is disabled while busy
#'
#' @inheritParams bslib::input_task_button
#' @param id (`character(1)`) the id of the button.
#' @param label (`character(1)`) the label of the button.
#' @param icon (`character(1)` or `NULL`) the name of the Bootstrap icon to be
#' displayed on the button.
#' @param additional_class (`character(1)` or `NULL`) additional CSS class to be
#' added to the button.
#'
#' @return A `shiny` action button that is disabled while busy.
#'
#' @examples
#' library(shiny)
#'
#' ui <- fluidPage(
#'   sliderInput("obs", "Number of observations", 0, 1000, 500),
#'   action_button("goButton", "Go!", class = "btn-success"),
#'   plotOutput("distPlot")
#' )
#'
#' server <- function(input, output) {
#'   output$distPlot <- renderPlot({
#'     # Take a dependency on input$goButton. This will run once initially,
#'     # because the value changes from NULL to 0.
#'     input$goButton
#'
#'     # Use isolate() to avoid dependency on input$obs
#'     dist <- isolate(rnorm(input$obs))
#'     hist(dist)
#'   })
#' }
#'
#' shinyApp(ui, server)
#'
#' @export
action_button_with_busy <- function(id,
                                    label,
                                    icon = NULL,
                                    type = "primary",
                                    outline = FALSE,
                                    additional_class = NULL) {
  checkmate::assert_string(type)
  checkmate::assert_string(additional_class, null.ok = TRUE)
  shiny::tagList(
    shinyjs::useShinyjs(),
    htmltools::htmlDependency(
      name = "teal-widgets-action-button",
      version = utils::packageVersion("teal.widgets"),
      package = "teal.widgets",
      src = "css",
      stylesheet = "action_button.css"
    ),
    htmltools::htmlDependency(
      name = "teal-widgets-busy-disable",
      version = utils::packageVersion("teal.widgets"),
      package = "teal.widgets",
      src = "js",
      script = "busy-disable.js"
    ),
    shiny::tags$button(
      id = id,
      class = sprintf(
        "teal-widgets action-button teal-widgets-busy-disable btn btn-%1$s %1$s %2$s %3$s",
        trimws(type),
        ifelse(isTRUE(outline), "outline-button", ""),
        additional_class %||% ""
      ),
      role = "button",
      style = "text-decoration: none;",
      if (!is.null(icon)) {
        margin_style <- ifelse(is.null(label), "margin: 0 10px 0 10px;", "")
        shiny::tags$span(
          style = margin_style,
          bsicons::bs_icon(icon, class = sprintf("text-%s", type))
        )
      },
      label
    )
  )
}

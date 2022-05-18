#' @title Draggable Buckets
#' @description
#' A custom widget with draggable elements that can be put into buckets.
#'
#' @param input_id (`character(1)`) the `HTML` id of this widget
#' @param label (`character(1)`) the header of this widget
#' @param elements (`character`) the elements to drag into buckets
#' @param buckets (`character`) the names of the buckets the elements can but put in
#'
#' @return the `HTML` code comprising an instance of this widget
#' @export
#'
#' @examples
#' ui <- shiny::fluidPage(
#'   draggable_buckets("id", "Choices #1", c("a", "b"), c("bucket1", "bucket2")),
#'   draggable_buckets("id2", "Choices #2", letters, c("vowels", "consonants")),
#'   shiny::verbatimTextOutput("out")
#' )
#' server <- function(input, output) {
#'   shiny::observeEvent(input$id, {
#'     print(shiny::isolate(input$id))
#'   })
#'   output$out <- shiny::renderPrint(input$id)
#' }
#' if (interactive()) shiny::shinyApp(ui, server)
#'
draggable_buckets <- function(input_id, label, elements, buckets) {
  shiny::tagList(
    shiny::tags$head(
      shiny::singleton(shiny::includeScript(system.file("widgets/draggable_buckets.js", package = "teal.widgets")))
    ),
    shiny::tags$head(shiny::singleton(
      shiny::includeCSS(system.file("css/draggable_buckets.css", package = "teal.widgets"))
    )),
    shiny::div(
      shiny::tags$span(label),
      shiny::tags$div(
        lapply(seq_along(elements), function(index) {
          render_draggable_element(value = elements[index], id = paste0(input_id, "draggable", index))
        }),
        id = "elements",
        class = c("form-control", "elements"),
        ondragover = "allowDrop(event)",
        ondrop = "drop(event)"
      ),
      shiny::tags$div(lapply(buckets, render_bucket)),
      class = "draggableBuckets",
      id = input_id
    )
  )
}

render_draggable_element <- function(value, id) {
  shiny::tags$div(value, id = id, class = "element", draggable = "true", ondragstart = "drag(event)", ondrop = "false")
}

render_bucket <- function(name) {
  shiny::tags$div(
    shiny::tags$div(paste0(name, ":")),
    class = c("form-control", "bucket"), ondragover = "allowDrop(event)", ondrop = "drop(event)", `data-label` = name
  )
}

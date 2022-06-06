#' @title Draggable Buckets
#' @description  `r lifecycle::badge("experimental")`
#' A custom widget with draggable elements that can be put into buckets.
#'
#' @param input_id (`character(1)`) the `HTML` id of this widget
#' @param label (`character(1)` or `shiny.tag`) the header of this widget
#' @param elements (`character`) the elements to drag into buckets
#' @param buckets (`character` or `list`) the names of the buckets the elements can be put in or a list of key-pair
#' values where key is a name of a bucket and value is a character vector of elements in a bucket
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
#' # With default elements in the bucket
#' ui <- shiny::fluidPage(
#'   draggable_buckets("id", "Choices #1", c("a", "b"), list(bucket1 = character(), bucket2 = c("c"))),
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
draggable_buckets <- function(input_id, label, elements = character(), buckets) {
  checkmate::assert_string(input_id)
  checkmate::assert_true(inherits(label, "character") || inherits(label, "shiny.tag"))
  checkmate::assert_character(c(elements, unlist(buckets)), min.len = 0, null.ok = TRUE, unique = TRUE)
  checkmate::assert(
    checkmate::check_character(buckets, min.len = 1),
    checkmate::check_list(buckets, types = "character", names = "unique")
  )

  elements_iterator <- new.env(parent = emptyenv())
  elements_iterator$it <- 0

  buckets <- `if`(
    is.list(buckets),
    lapply(names(buckets), function(bucket_name) {
      render_bucket(
        name = bucket_name,
        elements = buckets[[bucket_name]],
        elements_iterator = elements_iterator,
        input_id = input_id
      )
    }),
    lapply(buckets, render_bucket)
  )
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
        lapply(elements, function(element) {
          elements_iterator$it <- elements_iterator$it + 1
          render_draggable_element(value = element, id = paste0(input_id, "draggable", elements_iterator$it))
        }),
        id = paste0(input_id, "elements"),
        class = c("form-control", "elements"),
        ondragover = "allowDrop(event)",
        ondrop = "drop(event)"
      ),
      shiny::tagList(buckets),
      class = "draggableBuckets",
      id = input_id
    )
  )
}

render_draggable_element <- function(value, id) {
  shiny::tags$div(
    value,
    id = id,
    class = "element",
    draggable = "true",
    ondragstart = "drag(event)",
    ondragover = "allowDrop(event)",
    ondrop = "dropReorder(event)"
  )
}

render_bucket <- function(name, elements = NULL, elements_iterator = NULL, input_id = NULL) {
  shiny::tags$div(
    shiny::tags$div(
      paste0(name, ":"),
      class = "bucket-name",
      ondragover = "allowDrop(event)",
      ondrop = "dropBucketName(event)"
    ),
    lapply(elements, function(element) {
      elements_iterator$it <- elements_iterator$it + 1
      render_draggable_element(element, id = paste0(input_id, "draggable", elements_iterator$it))
    }),
    class = c("form-control", "bucket"),
    ondragover = "allowDrop(event)",
    ondrop = "drop(event)",
    `data-label` = name
  )
}

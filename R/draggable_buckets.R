#' @keywords internal
#' @noRd
draggable_buckets_deps <- function() {
  htmltools::htmlDependency(
    name = "teal-widgets-draggable-buckets",
    version = utils::packageVersion("teal.widgets"),
    package = "teal.widgets",
    src = "draggable-buckets",
    script = "draggable-buckets.js",
    stylesheet = "draggable-buckets.css"
  )
}

#' @title Draggable Buckets
#' @description
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
#' @details `shinyvalidate` validation can be used with this widget. See example below.
#'
#' @examples
#' library(shiny)
#'
#' ui <- bslib::page_fluid(
#'   draggable_buckets("id", "Choices #1", c("a", "b"), c("bucket1", "bucket2")),
#'   draggable_buckets("id2", "Choices #2", letters, c("vowels", "consonants")),
#'   verbatimTextOutput("out"),
#'   verbatimTextOutput("out2")
#' )
#' server <- function(input, output) {
#'   iv <- shinyvalidate::InputValidator$new()
#'   iv$add_rule(
#'     "id",
#'     function(data) if (length(data[["bucket1"]]) == 0) "There should be stuff in bucket 1"
#'   )
#'   iv$enable()
#'
#'   observeEvent(list(input$id, input$id2), {
#'     print(isolate(input$id))
#'     print(isolate(input$id2))
#'   })
#'   output$out <- renderPrint({
#'     iv$is_valid()
#'     input$id
#'   })
#'   output$out2 <- renderPrint(input$id2)
#' }
#' if (interactive()) shinyApp(ui, server)
#'
#' # With default elements in the bucket
#' ui <- bslib::page_fluid(
#'   draggable_buckets("id", "Choices #1", c("a", "b"), list(bucket1 = character(), bucket2 = c("c"))),
#'   verbatimTextOutput("out")
#' )
#' server <- function(input, output) {
#'   observeEvent(input$id, {
#'     print(isolate(input$id))
#'   })
#'   output$out <- renderPrint(input$id)
#' }
#' if (interactive()) shinyApp(ui, server)
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

  shiny::tagList(
    draggable_buckets_deps(),
    shiny::div(
      tags$span(label),
      render_unbucketed_elements(elements = elements, elements_iterator = elements_iterator, widget_id = input_id),
      render_buckets(buckets = buckets, elements_iterator = elements_iterator, widget_id = input_id),
      class = "draggableBuckets",
      id = input_id
    )
  )
}

render_unbucketed_elements <- function(elements, elements_iterator, widget_id) {
  tags$div(
    lapply(elements, function(element) {
      elements_iterator$it <- elements_iterator$it + 1
      render_draggable_element(
        value = element,
        id = paste0(widget_id, "draggable", elements_iterator$it),
        widget_id = widget_id
      )
    }),
    id = paste0(widget_id, "elements"),
    class = c("form-control", "elements"),
    ondragover = "allowDrop(event)",
    ondrop = "drop(event)",
    `data-widget` = widget_id
  )
}

render_buckets <- function(buckets, elements_iterator, widget_id) {
  buckets <- `if`(
    is.list(buckets),
    lapply(names(buckets), function(bucket_name) {
      render_bucket(
        name = bucket_name,
        elements = buckets[[bucket_name]],
        elements_iterator = elements_iterator,
        widget_id = widget_id
      )
    }),
    lapply(buckets, render_bucket, widget_id = widget_id, elements_iterator = elements_iterator)
  )
  shiny::tagList(buckets)
}

render_draggable_element <- function(value, id, widget_id) {
  tags$div(
    value,
    id = id,
    class = "element",
    draggable = "true",
    ondragstart = "drag(event)",
    ondragover = "allowDrop(event)",
    ondrop = "dropReorder(event)",
    `data-widget` = widget_id
  )
}

render_bucket <- function(name, elements = NULL, elements_iterator = NULL, widget_id = NULL) {
  tags$div(
    tags$div(
      paste0(name, ":"),
      class = "bucket-name",
      ondragover = "allowDrop(event)",
      ondrop = "dropBucketName(event)",
      `data-widget` = widget_id
    ),
    lapply(elements, function(element) {
      elements_iterator$it <- elements_iterator$it + 1
      render_draggable_element(
        value = element,
        id = paste0(widget_id, "draggable", elements_iterator$it),
        widget_id = widget_id
      )
    }),
    class = c("form-control", "bucket"),
    ondragover = "allowDrop(event)",
    ondrop = "drop(event)",
    `data-label` = name,
    `data-widget` = widget_id
  )
}

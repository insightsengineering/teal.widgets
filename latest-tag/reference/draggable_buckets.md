# Draggable Buckets

A custom widget with draggable elements that can be put into buckets.

## Usage

``` r
draggable_buckets(input_id, label, elements = character(), buckets)
```

## Arguments

- input_id:

  (`character(1)`) the `HTML` id of this widget

- label:

  (`character(1)` or `shiny.tag`) the header of this widget

- elements:

  (`character`) the elements to drag into buckets

- buckets:

  (`character` or `list`) the names of the buckets the elements can be
  put in or a list of key-pair values where key is a name of a bucket
  and value is a character vector of elements in a bucket

## Value

the `HTML` code comprising an instance of this widget

## Details

`shinyvalidate` validation can be used with this widget. See example
below.

## Examples

``` r
library(shiny)

ui <- bslib::page_fluid(
  draggable_buckets("id", "Choices #1", c("a", "b"), c("bucket1", "bucket2")),
  draggable_buckets("id2", "Choices #2", letters, c("vowels", "consonants")),
  verbatimTextOutput("out"),
  verbatimTextOutput("out2")
)
server <- function(input, output) {
  iv <- shinyvalidate::InputValidator$new()
  iv$add_rule(
    "id",
    function(data) if (length(data[["bucket1"]]) == 0) "There should be stuff in bucket 1"
  )
  iv$enable()

  observeEvent(list(input$id, input$id2), {
    print(isolate(input$id))
    print(isolate(input$id2))
  })
  output$out <- renderPrint({
    iv$is_valid()
    input$id
  })
  output$out2 <- renderPrint(input$id2)
}
if (interactive()) shinyApp(ui, server)

# With default elements in the bucket
ui <- bslib::page_fluid(
  draggable_buckets("id", "Choices #1", c("a", "b"), list(bucket1 = character(), bucket2 = c("c"))),
  verbatimTextOutput("out")
)
server <- function(input, output) {
  observeEvent(input$id, {
    print(isolate(input$id))
  })
  output$out <- renderPrint(input$id)
}
if (interactive()) shinyApp(ui, server)
```

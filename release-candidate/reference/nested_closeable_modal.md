# Nested Closeable Modal Popup

**\[deprecated\]** Alternative to
[`shiny::modalDialog`](https://rdrr.io/pkg/shiny/man/modalDialog.html).
Create a nested modal popup that can be shown/hidden using `jQuery` and
modal `id`, without disturbing the parent modal.

## Usage

``` r
nested_closeable_modal(id, ..., modal_args = list(easyClose = TRUE))
```

## Arguments

- id:

  (`character(1)`) `shiny` module id for the component.  
  Note that this id can be used to show/hide this modal with the
  appended `jQuery` methods show/hide.

- ...:

  (`shiny.tag`) `shiny` UI elements that will be displayed in the modal
  UI

- modal_args:

  (`list`) optional list of arguments for the
  [`shiny::modalDialog`](https://rdrr.io/pkg/shiny/man/modalDialog.html)
  function to customize the modal. Has `easyClose` set to `TRUE` as
  default

## Value

(`shiny.tag`) returns `HTML` for `shiny` module UI which can be nested
into a modal popup

## Examples

``` r
library(shiny)
library(shinyjs)
#> 
#> Attaching package: ‘shinyjs’
#> The following object is masked from ‘package:shiny’:
#> 
#>     runExample
#> The following objects are masked from ‘package:methods’:
#> 
#>     removeClass, show

ui <- bslib::page_fluid(
  useShinyjs(),
  actionButton("show_1", "$(\"#modal_1\").modal(\"show\")"),
  nested_closeable_modal(
    "modal_1",
    modal_args = list(
      size = "l",
      title = "First Modal",
      easyClose = TRUE,
      footer = NULL
    ),
    tags$div(
      "This modal can be closed by running", tags$code("$(\"#modal_1\").modal(\"hide\")"),
      "in the JS console!",
      tags$br(),
      "Note that the second modal is placed right within this modal",
      tags$br(),
      "Alternatively, calling the", tags$code("removeModal()"),
      "will remove all the active modal popups",
      tags$br(), tags$br(),
      actionButton("show_2", "$(\"#modal_2\").modal(\"show\")"),
      actionButton("hide_1", "$(\"#modal_1\").modal(\"hide\")"),
      nested_closeable_modal(
        id = "modal_2",
        modal_args = list(
          size = "m",
          title = "Second Modal",
          footer = NULL,
          easyClose = TRUE
        ),
        tags$div(
          "This modal can be closed by running", tags$code("$(\"#modal_1\").modal(\"hide\")"),
          "in the JS console!",
          "Note that removing the parent will remove the child.
           But, reopening will remember the open state of child",
          actionButton("hide_2", "$(\"#modal_2\").modal(\"hide\")"),
          actionButton("hide_all", "$(\"#modal_1\").modal(\"hide\")")
        )
      )
    )
  )
)
#> Warning: `nested_closeable_modal()` was deprecated in teal.widgets 0.5.0.

server <- function(input, output) {
  observeEvent(input$show_1, {
    runjs("$(\"#modal_1\").modal(\"show\")")
  })
  observeEvent(input$show_2, {
    runjs("$(\"#modal_2\").modal(\"show\")")
  })
  observeEvent(c(input$hide_1, input$hide_all), {
    runjs("$(\"#modal_1\").modal(\"hide\")")
  })
  observeEvent(input$hide_2, {
    runjs("$(\"#modal_2\").modal(\"hide\")")
  })
}

if (interactive()) {
  shinyApp(ui, server)
}
```

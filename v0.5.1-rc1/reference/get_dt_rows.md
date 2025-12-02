# Map `lenghtMenu` property

Maps the `lengthMenu` selected value property of
[`DT::datatable`](https://rdrr.io/pkg/DT/man/datatable.html) to a
`shiny` variable.

## Usage

``` r
get_dt_rows(dt_name, dt_rows)
```

## Arguments

- dt_name:

  `ns()` of `inputId` of the
  [`DT::datatable`](https://rdrr.io/pkg/DT/man/datatable.html)

- dt_rows:

  `ns()` of `inputId` of the variable that holds the current selected
  value of `lengthMenu`

## Value

([`shiny::tagList`](https://rstudio.github.io/htmltools/reference/tagList.html))
A `shiny tagList`.

## Examples

``` r
library(shiny)
library(DT)
#> 
#> Attaching package: ‘DT’
#> The following objects are masked from ‘package:shiny’:
#> 
#>     dataTableOutput, renderDataTable

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
    # Change rows selected to see the first line on the UI change
    rows <- reactive({
      paste0("Selected Rows ", input$dt_rows)
    })
    output$rows <- renderText(rows())
  })
}
if (interactive()) {
  shinyApp(
    ui = ui("my_table_module"),
    server = function(input, output, session) server("my_table_module")
  )
}
```

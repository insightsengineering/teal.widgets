# `table_with_settings` module

Module designed to create a `shiny` table output based on table objects.
Supports `rtables` objects (`ElementaryTable` or `TableTree`),
`gtsummary` objects, or `gt` objects.

## Usage

``` r
table_with_settings_ui(id, ...)

table_with_settings_srv(id, table_r, show_hide_signal = reactive(TRUE))
```

## Arguments

- id:

  An ID string that corresponds with the ID used to call the module's UI
  function.

- ...:

  (`character`)  
  Useful for providing additional HTML classes for the output tag.

- table_r:

  (`reactive`)  
  reactive expression that yields a table object. Supported types:

  - `rtables` objects (`ElementaryTable` or `TableTree`)

  - `gtsummary` objects

  - `gt` objects (`gt_tbl`)

- show_hide_signal:

  (`reactive logical`) optional  
  mechanism to allow modules which call this module to show/hide the
  table_with_settings UI.

## Value

A `shiny` module.

## Examples

``` r
library(shiny)
library(rtables)
#> Loading required package: formatters
#> 
#> Attaching package: ‘formatters’
#> The following object is masked from ‘package:base’:
#> 
#>     %||%
#> Loading required package: magrittr
#> 
#> Attaching package: ‘magrittr’
#> The following objects are masked from ‘package:testthat’:
#> 
#>     equals, is_less_than, not
#> 
#> Attaching package: ‘rtables’
#> The following object is masked from ‘package:utils’:
#> 
#>     str
library(gtsummary)
library(gt)
#> 
#> Attaching package: ‘gt’
#> The following object is masked from ‘package:shinyjs’:
#> 
#>     html
library(magrittr)

ui <- bslib::page_fluid(
  table_with_settings_ui(id = "rtables_table"),
  table_with_settings_ui(id = "gtsummary_table"),
  table_with_settings_ui(id = "gt_table")
)

server <- function(input, output, session) {
  table_r_rtables <- reactive({
    l <- basic_table() %>%
      split_cols_by("ARM") %>%
      analyze(c("SEX", "AGE"))
    build_table(l, DM)
  })

  table_r_gtsummary <- reactive({
    gtsummary::tbl_summary(mtcars)
  })

  table_r_gt <- reactive({
    mtcars %>%
      gt::gt() %>%
      gt::tab_header(title = "Motor Trend Car Road Tests")
  })

  table_with_settings_srv(id = "rtables_table", table_r = table_r_rtables)
  table_with_settings_srv(id = "gtsummary_table", table_r = table_r_gtsummary)
  table_with_settings_srv(id = "gt_table", table_r = table_r_gt)
}

if (interactive()) {
  shinyApp(ui, server)
}
```

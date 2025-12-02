# `table_with_settings` module

Module designed to create a `shiny` table output based on `rtable`
object (`ElementaryTable` or `TableTree`) input.

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
  reactive expression that yields an `rtable` object (`ElementaryTable`
  or `TableTree`)

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
library(magrittr)

ui <- bslib::page_fluid(
  table_with_settings_ui(
    id = "table_with_settings"
  )
)

server <- function(input, output, session) {
  table_r <- reactive({
    l <- basic_table() %>%
      split_cols_by("ARM") %>%
      analyze(c("SEX", "AGE"))

    tbl <- build_table(l, DM)

    tbl
  })

  table_with_settings_srv(id = "table_with_settings", table_r = table_r)
}

if (interactive()) {
  shinyApp(ui, server)
}
```

library(shiny)
library(rtables)
library(magrittr)
library(teal.widgets)

shinyApp(
  ui = fluidPage(
    table_with_settings_ui(
      id = "table_with_settings"
    )
  ),
  server = function(input, output, session) {
    table_r <- reactive({
      l <- basic_table() %>%
        split_cols_by("ARM") %>%
        analyze(c("SEX", "AGE"))

      tbl <- build_table(l, DM)

      tbl
    })
    table_with_settings_srv(id = "table_with_settings", table_r = table_r)
  }
)

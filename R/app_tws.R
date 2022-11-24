app_tws <- function() {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      table_with_settings_ui(
        id = "table_with_settings"
      )
    ),
    server = function(input, output, session) {
      df1 <- data.frame(
        AGE = c(35, 41),
        SEX = factor(c("M", "F")),
        ARM = c("B: Placebo", "C: Combination")
      )

      table_r <- shiny::reactive({
        l <- rtables::basic_table() %>%
          rtables::split_cols_by("ARM") %>%
          rtables::analyze(c("SEX", "AGE"))

        tbl <- rtables::build_table(l, df1)

        tbl
      })
      table_with_settings_srv(id = "table_with_settings", table_r = table_r)
    }
  )
}

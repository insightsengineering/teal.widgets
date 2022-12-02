#' Table with settings app
#'
#' @description Example table with setting app for testing using \code{shinytest2}
#'
#' @keywords internal
#'
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

#' @keywords internal
is_draw <- function(plot_fun) {
  checkmate::assert_function(plot_fun)
  graphics.off() # close any current graphics devices
  cdev <- dev.cur()
  plot_fun()
  if (cdev != dev.cur()) {
    on.exit(dev.off())
    return(TRUE)
  }
  return(FALSE)
}

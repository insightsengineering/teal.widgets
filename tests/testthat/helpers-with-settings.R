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
        l1 <- rtables::basic_table()
        l2 <- rtables::split_cols_by(l1, "ARM")
        l3 <- rtables::analyze(l2, c("SEX", "AGE"))
        tbl <- rtables::build_table(l3, df1)
        tbl
      })
      table_with_settings_srv(id = "table_with_settings", table_r = table_r)
    }
  )
}

#' Plot with settings app
#'
#' @description Example plot with setting app for testing using \code{shinytest2}
#'
#' @keywords internal
#'
app_pws <- function() {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shinyjs::useShinyjs(),
      shiny::actionButton("button", "Show/Hide"),
      plot_with_settings_ui(
        id = "plot_with_settings"
      )
    ),
    server = function(input, output, session) {
      plot_r <- shiny::reactive({
        ggplot2::ggplot(data.frame(x = 1:5, y = 1:5)) +
          ggplot2::geom_point(ggplot2::aes(x = 1:5, y = 1:5))
      })

      show_hide_signal <- shiny::reactiveVal(TRUE)

      shiny::observeEvent(input$button, {
        show_hide_signal(
          !show_hide_signal()
        )
      })

      plot_data <- plot_with_settings_srv(
        id = "plot_with_settings",
        plot_r = plot_r,
        height = c(400, 100, 1200),
        width = c(500, 250, 750),
        brushing = TRUE,
        clicking = TRUE,
        dblclicking = TRUE,
        hovering = TRUE,
        show_hide_signal = show_hide_signal
      )

      shiny::exportTestValues(
        plot_r = plot_r,
        plot_data = plot_data
      )
    }
  )
}

#' Function to check if a function has a side effect of drawing something
#' @param `function` function which possibly draws something.
#' @return `logical(1)` whether the function has a side effect of drawing a plot.
#' @note reference to https://stackoverflow.com/questions/74615694/check-if-a-function-draw-plot-something
#' @keywords internal
is_draw <- function(plot_fun) {
  checkmate::assert_function(plot_fun)
  grDevices::graphics.off() # close any current graphics devices
  cdev <- grDevices::dev.cur()
  plot_fun()
  if (cdev != grDevices::dev.cur()) {
    on.exit(grDevices::dev.off())
    return(TRUE)
  }
  return(FALSE)
}

app_pws <- function() {

library(shiny)
library(teal.widgets)

shinyApp(
  ui = fluidPage(
    shinyjs::useShinyjs(),
    actionButton("button", "Show/Hide"),
    plot_with_settings_ui(
      id = "plot_with_settings"
    )
  ),
  server = function(input, output, session) {
    plot_r <- reactive({
      ggplot2::ggplot(data.frame(x = 1:5, y = 1:5)) +
        ggplot2::geom_point(ggplot2::aes(x = 1:5, y = 1:5))
    })

    show_hide_signal <- reactiveVal(TRUE)

    observeEvent(input$button, {
      show_hide_signal(
        isolate(
          !show_hide_signal()
        )
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

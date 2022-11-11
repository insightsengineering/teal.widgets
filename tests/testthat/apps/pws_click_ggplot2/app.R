library(shiny)
library(teal.widgets)

shinyApp(
  ui = fluidPage(
    plot_with_settings_ui(
      id = "plot_with_settings"
    ),
    fluidRow(
      column(4, h3("Brush"), verbatimTextOutput("brushing_data")),
      column(4, h3("Click"), verbatimTextOutput("clicking_data")),
      column(4, h3("DblClick"), verbatimTextOutput("dblclicking_data")),
      column(4, h3("Hover"), verbatimTextOutput("hovering_data"))
    )
  ),
  server = function(input, output, session) {
    plot_r <- reactive({
      ggplot2::ggplot(data.frame(x = 1:5, y = 1:5)) +
        ggplot2::geom_point(ggplot2::aes(x = x, y = y))
    })

    plot_data <- plot_with_settings_srv(
      id = "plot_with_settings",
      plot_r = plot_r,
      height = c(400, 100, 1200),
      brushing = TRUE,
      clicking = TRUE,
      dblclicking = TRUE,
      hovering = TRUE
    )

    output$brushing_data <- renderPrint(plot_data$brush())
    output$clicking_data <- renderPrint(plot_data$click())
    output$dblclicking_data <- renderPrint(plot_data$dblclick())
    output$hovering_data <- renderPrint(plot_data$hover())
  }
)

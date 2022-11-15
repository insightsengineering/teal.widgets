library(shiny)
library(teal.widgets)
library(tern)
library(ggplot2)

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
      plots <- list(ggplot2::ggplot() +
                      ggplot2::aes(x = 1:10) +
                      ggplot2::geom_bar(),
                    ggplot2::ggplot() +
                      ggplot2::aes(x = 10:1, y = 1:10) +
                      ggplot2::geom_point())
      p <- tern::stack_grobs(grobs = lapply(plots, ggplot2::ggplotGrob))
      grid::grid.newpage()
      grid::grid.draw(p)
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

    shiny::exportTestValues(
      plot_r = plot_r,
      plot_data = plot_data
    )
  }
)

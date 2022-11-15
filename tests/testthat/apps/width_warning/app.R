library(shiny)
library(teal.widgets)

shinyApp(
  ui = fluidPage(
    plot_with_settings_ui(
      id = "plot_with_settings"
    )
  ),
  server = function(input, output, session) {
    plot_r <- reactive({
      plot(1:5, 1:5)
    })

    plot_with_settings_srv(
      id = "plot_with_settings",
      plot_r = plot_r,
      height = c(400, 100, 1200),
      width = c(500, 250, 750)
    )
  }
)

library(shiny)
library(teal.widgets)
library(shinyjs)

shinyApp(
  ui = fluidPage(
    shinyjs::useShinyjs(),
    plot_with_settings_ui(
      id = "plot_with_settings"
    ),
    actionButton("button", "Show/Hide")
  ),
  server = function(input, output, session) {
    plot_r <- reactive(ggplot2::qplot(x = 1, y = 1))

    show_hide_signal_rv <- reactiveVal(TRUE)

    observeEvent(input$button, show_hide_signal_rv(!show_hide_signal_rv()))

    plot_with_settings_srv(
      id = "plot_with_settings",
      plot_r = plot_r,
      height = c(400, 100, 1200),
      width = c(500, 250, 750),
      show_hide_signal = reactive(show_hide_signal_rv())
    )
  }
)

# Plot-with-settings module

Universal module for plots with settings for height, width, and
download.

## Usage

``` r
plot_with_settings_ui(id)

plot_with_settings_srv(
  id,
  plot_r,
  height = c(600, 200, 2000),
  width = NULL,
  show_hide_signal = reactive(TRUE),
  brushing = FALSE,
  clicking = FALSE,
  dblclicking = FALSE,
  hovering = FALSE,
  graph_align = "left"
)
```

## Arguments

- id:

  (`character(1)`) `shiny` module id.

- plot_r:

  (`reactive` or `function`)  
  `reactive` expression or a simple `function` to draw a plot. A simple
  `function` is needed e.g. for base plots like `plot(1)` as the output
  can not be caught when downloading. Take into account that simple
  functions are less efficient than reactive, as not catching the
  result.

- height:

  (`numeric`) optional  
  vector with three elements c(VAL, MIN, MAX), where VAL is the starting
  value of the slider in the main and expanded plot display.

- width:

  (`numeric`) optional  
  vector with three elements `c(VAL, MIN, MAX)`, where VAL is the
  starting value of the slider in the main and expanded plot display;
  `NULL` for default display.

- show_hide_signal:

  optional, (`reactive logical` a mechanism to allow modules which call
  this module to show/hide the plot_with_settings UI)

- brushing:

  (`logical`) optional  
  mechanism to enable / disable brushing on the main plot. All the
  brushing data is stored as a reactive object in the `"brush"` element
  of returned list. See the example for details.

- clicking:

  (`logical`)  
  a mechanism to enable / disable clicking on data points on the main
  plot. All the clicking data is stored as a reactive object in the
  `"click"` element of returned list. See the example for details.

- dblclicking:

  (`logical`) optional  
  mechanism to enable / disable double-clicking on data points on the
  main plot. All the double clicking data is stored as a reactive object
  in the the `"dblclick"` element of returned list. See the example for
  details.

- hovering:

  (`logical(1)`) optional  
  mechanism to enable / disable hovering over data points on the main
  plot. All the hovering data is stored as a reactive object in the
  `"hover"` element of returned list. See the example for details.

- graph_align:

  (`character(1)`) optional,  
  one of `"left"` (default), `"center"`, `"right"` or `"justify"`. The
  alignment of the graph on the main page.

## Value

A `shiny` module.

## Details

By default the plot is rendered with `72 dpi`. In order to change this,
to for example 96 set `options(teal.plot_dpi = 96)`. The minimum allowed
`dpi` value is `24` and it must be a whole number. If an invalid value
is set then the default value is used and a warning is outputted to the
console.

## Examples

``` r
# Example using a reactive as input to plot_r
library(shiny)
library(ggplot2)

ui <- bslib::page_fluid(
  plot_with_settings_ui(
    id = "plot_with_settings"
  )
)

server <- function(input, output, session) {
  plot_r <- reactive({
    ggplot(faithful, aes(x = .data$waiting, y = .data$eruptions)) +
      geom_point()
  })

  plot_with_settings_srv(
    id = "plot_with_settings",
    plot_r = plot_r,
    height = c(400, 100, 1200),
    width = c(500, 250, 750)
  )
}

if (interactive()) {
  shinyApp(ui, server)
}

# Example using a function as input to plot_r
library(lattice)

ui <- bslib::page_fluid(
  radioButtons("download_option", "Select the Option", list("ggplot", "trellis", "grob", "base")),
  plot_with_settings_ui(
    id = "plot_with_settings"
  ),
  sliderInput("nums", "Value", 1, 10, 1)
)

server <- function(input, output, session) {
  plot_r <- function() {
    numbers <- seq_len(input$nums)
    if (input$download_option == "ggplot") {
      ggplot(data.frame(n = numbers), aes(.data$n)) +
        geom_bar()
    } else if (input$download_option == "trellis") {
      densityplot(numbers)
    } else if (input$download_option == "grob") {
      tr_plot <- densityplot(numbers)
      ggplotGrob(
        ggplot(data.frame(n = numbers), aes(.data$n)) +
          geom_bar()
      )
    } else if (input$download_option == "base") {
      plot(numbers)
    }
  }

  plot_with_settings_srv(
    id = "plot_with_settings",
    plot_r = plot_r,
    height = c(400, 100, 1200),
    width = c(500, 250, 750)
  )
}

if (interactive()) {
  shinyApp(ui, server)
}

# Example with brushing/hovering/clicking/double-clicking
ui <- bslib::page_fluid(
  plot_with_settings_ui(
    id = "plot_with_settings"
  ),
  fluidRow(
    column(4, tags$h3("Brush"), verbatimTextOutput("brushing_data")),
    column(4, tags$h3("Click"), verbatimTextOutput("clicking_data")),
    column(4, tags$h3("DblClick"), verbatimTextOutput("dblclicking_data")),
    column(4, tags$h3("Hover"), verbatimTextOutput("hovering_data"))
  )
)

server <- function(input, output, session) {
  plot_r <- reactive({
    ggplot(faithful, aes(x = .data$waiting, y = .data$eruptions)) +
      geom_point()
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

if (interactive()) {
  shinyApp(ui, server)
}

# Example which allows module to be hidden/shown
library("shinyjs")

ui <- bslib::page_fluid(
  useShinyjs(),
  actionButton("button", "Show/Hide"),
  plot_with_settings_ui(
    id = "plot_with_settings"
  )
)

server <- function(input, output, session) {
  plot_r <- plot_r <- reactive(
    ggplot(faithful, aes(x = .data$waiting, y = .data$eruptions)) +
      geom_point()
  )

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

if (interactive()) {
  shinyApp(ui, server)
}
```

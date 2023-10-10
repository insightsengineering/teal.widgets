#' @rdname plot_with_settings
#' @export
plot_with_settings_ui <- function(id) {
  checkmate::assert_string(id)

  ns <- NS(id)

  tagList(
    shiny::singleton(shiny::tags$head(
      shiny::tags$script(
        # nolint start
        sprintf(
          '$(document).on("shiny:connected", function(e) {
            Shiny.onInputChange("%s", document.getElementById("%s").clientWidth);
            Shiny.onInputChange("%s", 0.87*window.innerWidth);
            //based on modal CSS property, also accounting for margins
          });
          $(window).resize(function(e) {
            Shiny.onInputChange("%s", document.getElementById("%s").clientWidth);
            Shiny.onInputChange("%s", 0.87*window.innerWidth);
            //based on modal CSS property, also accounting for margins
          });',
          # nolint end
          ns("flex_width"), # session input$ variable name
          ns("plot_out_main"), # graph parent id
          ns("plot_modal_width"), # session input$ variable name
          ns("flex_width"), # session input$ variable name
          ns("plot_out_main"), # graph parent id
          ns("plot_modal_width") # session input$ variable name
        )
      )
    )),
    include_css_files("plot_with_settings"),
    tags$div(
      id = ns("plot-with-settings"),
      tags$div(
        class = "plot-settings-buttons",
        type_download_ui(ns("downbutton")),
        actionButton(
          ns("expand"),
          label = character(0),
          icon = icon("up-right-and-down-left-from-center"),
          class = "btn-sm"
        ),
        shinyWidgets::dropdownButton(
          circle = FALSE,
          icon = icon("maximize"),
          inline = TRUE,
          right = TRUE,
          label = "",
          inputId = ns("expbut"),
          uiOutput(ns("slider_ui")),
          uiOutput(ns("width_warning"))
        )
      ),
      uiOutput(ns("plot_out_main"), class = "plot_out_container", width = "100%")
    )
  )
}

#' Plot-with-settings module
#'
#' @rdname plot_with_settings
#' @description `r lifecycle::badge("stable")`
#' Universal module for plots with settings for height, width, and download.
#'
#' @export
#'
#' @param id (`character(1)`) `shiny` module id.
#'
#' @param plot_r (`reactive` or `function`)\cr
#'  `reactive` expression or a simple `function` to draw a plot.
#'  A simple `function` is needed e.g. for base plots like `plot(1)` as the output can not be caught when downloading.
#'  Take into account that simple functions are less efficient than reactive, as not catching the result.
#' @param height (`numeric`, optional)\cr
#'  vector with three elements c(VAL, MIN, MAX), where VAL is the starting value of the slider in
#'  the main and modal plot display. The value in the modal display is taken from the value of the
#'  slider in the main plot display.
#' @param width (`numeric`, optional)\cr
#'  vector with three elements `c(VAL, MIN, MAX)`, where VAL is the starting value of the slider in
#'  the main and modal plot display; `NULL` for default display. The value in the modal
#'  display is taken from the value of the slider in the main plot display.
#' @param show_hide_signal optional, (\code{reactive logical} a mechanism to allow modules which call this
#'     module to show/hide the plot_with_settings UI)
#' @param brushing (`logical`, optional)\cr
#'  a mechanism to enable / disable brushing on the main plot (in particular: not the one displayed
#'  in modal). All the brushing data is stored as a reactive object in the `"brush"` element of
#'  returned list. See the example for details.
#' @param clicking (`logical`)\cr
#'  a mechanism to enable / disable clicking on data points on the main plot (in particular: not the
#'  one displayed in modal). All the clicking data is stored as a reactive object in the `"click"`
#'  element of returned list. See the example for details.
#' @param dblclicking (`logical`, optional)\cr
#'  a mechanism to enable / disable double-clicking on data points on the main plot (in particular:
#'  not the one displayed in modal). All the double clicking data is stored as a reactive object in
#'  the `"dblclick"` element of returned list. See the example for details.
#' @param hovering (`logical(1)`, optional)\cr
#'  a mechanism to enable / disable hovering over data points on the main plot (in particular: not
#'  the one displayed in modal). All the hovering data is stored as a reactive object in the
#' `"hover"` element of returned list. See the example for details.
#' @param graph_align (`character(1)`, optional)\cr
#'  one of `"left"` (default), `"center"`, `"right"` or `"justify"`. The alignment of the graph on
#'  the main page.
#'
#' @details By default the plot is rendered with `72 dpi`. In order to change this, to for example 96 set
#' `options(teal.plot_dpi = 96)`. The minimum allowed `dpi` value is `24` and it must be a whole number.
#' If an invalid value is set then the default value is used and a warning is outputted to the console.
#'
#' @return A `shiny` module.
#'
#' @examples
#' # Example using a reactive as input to plot_r
#' library(shiny)
#' app1 <- shinyApp(
#'   ui = fluidPage(
#'     plot_with_settings_ui(
#'       id = "plot_with_settings"
#'     )
#'   ),
#'   server = function(input, output, session) {
#'     plot_r <- reactive({
#'       ggplot2::qplot(x = 1, y = 1)
#'     })
#'
#'     plot_with_settings_srv(
#'       id = "plot_with_settings",
#'       plot_r = plot_r,
#'       height = c(400, 100, 1200),
#'       width = c(500, 250, 750)
#'     )
#'   }
#' )
#'
#' if (interactive()) {
#'   runApp(app1)
#' }
#'
#' # Example using a function as input to plot_r
#' app2 <- shinyApp(
#'   ui = fluidPage(
#'     radioButtons("download_option", "Select the Option", list("ggplot", "trellis", "grob", "base")),
#'     plot_with_settings_ui(
#'       id = "plot_with_settings"
#'     ),
#'     sliderInput("nums", "Value", 1, 10, 1)
#'   ),
#'   server = function(input, output, session) {
#'     plot_r <- function() {
#'       numbers <- seq_len(input$nums)
#'       if (input$download_option == "ggplot") {
#'         ggplot2::ggplot(data.frame(n = numbers), ggplot2::aes(n)) +
#'           ggplot2::geom_bar()
#'       } else if (input$download_option == "trellis") {
#'         lattice::densityplot(numbers)
#'       } else if (input$download_option == "grob") {
#'         tr_plot <- lattice::densityplot(numbers)
#'         ggplot2::ggplotGrob(
#'           ggplot2::ggplot(data.frame(n = numbers), ggplot2::aes(n)) +
#'             ggplot2::geom_bar()
#'         )
#'       } else if (input$download_option == "base") {
#'         plot(numbers)
#'       }
#'     }
#'
#'     plot_with_settings_srv(
#'       id = "plot_with_settings",
#'       plot_r = plot_r,
#'       height = c(400, 100, 1200),
#'       width = c(500, 250, 750)
#'     )
#'   }
#' )
#'
#' if (interactive()) {
#'   runApp(app2)
#' }
#'
#' # Example with brushing/hovering/clicking/double-clicking
#' app3 <- shinyApp(
#'   ui = fluidPage(
#'     plot_with_settings_ui(
#'       id = "plot_with_settings"
#'     ),
#'     fluidRow(
#'       column(4, h3("Brush"), verbatimTextOutput("brushing_data")),
#'       column(4, h3("Click"), verbatimTextOutput("clicking_data")),
#'       column(4, h3("DblClick"), verbatimTextOutput("dblclicking_data")),
#'       column(4, h3("Hover"), verbatimTextOutput("hovering_data"))
#'     )
#'   ),
#'   server = function(input, output, session) {
#'     plot_r <- reactive({
#'       ggplot2::qplot(x = 1:5, y = 1:5)
#'     })
#'
#'     plot_data <- plot_with_settings_srv(
#'       id = "plot_with_settings",
#'       plot_r = plot_r,
#'       height = c(400, 100, 1200),
#'       brushing = TRUE,
#'       clicking = TRUE,
#'       dblclicking = TRUE,
#'       hovering = TRUE
#'     )
#'
#'     output$brushing_data <- renderPrint(plot_data$brush())
#'     output$clicking_data <- renderPrint(plot_data$click())
#'     output$dblclicking_data <- renderPrint(plot_data$dblclick())
#'     output$hovering_data <- renderPrint(plot_data$hover())
#'   }
#' )
#'
#' if (interactive()) {
#'   runApp(app3)
#' }
#'
#' # Example which allows module to be hidden/shown
#' library("shinyjs")
#'
#' app4 <- shinyApp(
#'   ui = fluidPage(
#'     useShinyjs(),
#'     actionButton("button", "Show/Hide"),
#'     plot_with_settings_ui(
#'       id = "plot_with_settings"
#'     )
#'   ),
#'   server = function(input, output, session) {
#'     plot_r <- reactive(ggplot2::qplot(data = faithful, x = waiting, y = eruptions))
#'
#'     show_hide_signal_rv <- reactiveVal(TRUE)
#'
#'     observeEvent(input$button, show_hide_signal_rv(!show_hide_signal_rv()))
#'
#'     plot_with_settings_srv(
#'       id = "plot_with_settings",
#'       plot_r = plot_r,
#'       height = c(400, 100, 1200),
#'       width = c(500, 250, 750),
#'       show_hide_signal = reactive(show_hide_signal_rv())
#'     )
#'   }
#' )
#'
#' if (interactive()) {
#'   runApp(app4)
#' }
#'
plot_with_settings_srv <- function(id,
                                   plot_r,
                                   height = c(600, 200, 2000),
                                   width = NULL,
                                   show_hide_signal = reactive(TRUE),
                                   brushing = FALSE,
                                   clicking = FALSE,
                                   dblclicking = FALSE,
                                   hovering = FALSE,
                                   graph_align = "left") {
  checkmate::assert_string(id)
  checkmate::assert(
    checkmate::check_class(plot_r, "function"),
    checkmate::check_class(plot_r, "reactive")
  )
  checkmate::assert_numeric(height, min.len = 1, any.missing = FALSE)
  checkmate::assert_numeric(height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(height[1], lower = height[2], upper = height[3], .var.name = "height")
  checkmate::assert_numeric(width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(width[1], lower = width[2], upper = width[3], null.ok = TRUE, .var.name = "width")

  checkmate::assert_class(show_hide_signal, c("reactive", "function"))
  checkmate::assert_flag(brushing)
  checkmate::assert_flag(clicking)
  checkmate::assert_flag(dblclicking)
  checkmate::assert_flag(hovering)
  checkmate::assert_string(graph_align)
  checkmate::assert_subset(graph_align, c("left", "right", "center", "justify"))

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    default_w <- function() session$clientData[[paste0("output_", ns("plot_main_width"))]]
    default_h <- function() session$clientData[[paste0("output_", ns("plot_main_height"))]]

    default_slider_width <- reactiveVal(width)
    delayed_flex_width <- debounce(reactive(input$flex_width), millis = 100)

    if (is.null(width)) {
      # if width = NULL then set default_slider_width to be the value of the plot width on load
      observeEvent(session$clientData[[paste0("output_", ns("plot_main_width"))]],
        handlerExpr = {
          default_slider_width(default_w() * c(1, 0.5, 2.8))
        },
        once = TRUE,
        ignoreNULL = TRUE
      )

      observeEvent(delayed_flex_width(), {
        if (delayed_flex_width() > 0 && !isFALSE(input$width_resize_switch)) {
          default_slider_width(delayed_flex_width() * c(1, 0.5, 2.8))
          updateSliderInput(session, inputId = "width", value = delayed_flex_width())
        }
      })
    }

    plot_type <- reactive({
      if (inherits(plot_r(), "ggplot")) {
        "gg"
      } else if (inherits(plot_r(), "trellis")) {
        "trel"
      } else if (inherits(plot_r(), "grob")) {
        "grob"
      } else if (inherits(plot_r(), c("NULL", "histogram", "list")) && !inherits(plot_r, "reactive")) {
        "base"
      } else {
        "other"
      }
    })

    # allow modules which use this module to turn on and off the UI
    observeEvent(show_hide_signal(), {
      if (show_hide_signal()) {
        shinyjs::show("plot-with-settings")
      } else {
        shinyjs::hide("plot-with-settings")
      }
    })

    output$slider_ui <- renderUI({
      div(
        optionalSliderInputValMinMax(
          inputId = ns("height"),
          label = "Plot height",
          value_min_max = round(height),
          ticks = FALSE,
          step = 1L,
          round = TRUE
        ),
        tags$b("Plot width"),
        shinyWidgets::switchInput(
          inputId = ns("width_resize_switch"),
          onLabel = "ON",
          offLabel = "OFF",
          label = "Auto width",
          value = `if`(is.null(width), TRUE, FALSE),
          size = "mini",
          labelWidth = "80px"
        ),
        optionalSliderInputValMinMax(
          inputId = ns("width"),
          label = NULL,
          value_min_max = round(isolate(default_slider_width())),
          ticks = FALSE,
          step = 1L,
          round = TRUE
        )
      )
    })

    observeEvent(input$width_resize_switch | delayed_flex_width(), {
      if (length(input$width_resize_switch) && input$width_resize_switch) {
        shinyjs::disable("width")
        updateSliderInput(session, inputId = "width", value = delayed_flex_width())
      } else {
        shinyjs::enable("width")
      }
    })

    ranges <- reactiveValues(x = NULL, y = NULL)

    observeEvent(input$plot_dblclick, {
      brush <- input$plot_brush
      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)
      } else {
        ranges$x <- NULL
        ranges$y <- NULL
      }
    })

    p_height <- reactive(`if`(!is.null(input$height), input$height, height[1]))
    p_width <- reactive(`if`(!is.null(input$width), input$width, default_slider_width()[1]))
    output$plot_main <- renderPlot(
      apply_plot_modifications(
        plot_obj = plot_r(),
        plot_type = plot_type(),
        dblclicking = dblclicking,
        ranges = ranges
      ),
      res = get_plot_dpi(),
      height = p_height,
      width = p_width
    )

    output$plot_modal <- renderPlot(
      apply_plot_modifications(
        plot_obj = plot_r(),
        plot_type = plot_type(),
        dblclicking = dblclicking,
        ranges = ranges
      ),
      res = get_plot_dpi(),
      height = reactive(input$height_in_modal),
      width = reactive(input$width_in_modal)
    )

    output$plot_out_main <- renderUI({
      req(plot_r())
      div(
        align = graph_align,
        plotOutput(
          ns("plot_main"),
          height = "100%",
          brush = `if`(brushing, brushOpts(ns("plot_brush"), resetOnNew = FALSE), NULL),
          click = `if`(clicking, clickOpts(ns("plot_click")), NULL),
          dblclick = `if`(dblclicking, dblclickOpts(ns("plot_dblclick")), NULL),
          hover = `if`(hovering, hoverOpts(ns("plot_hover")), NULL)
        )
      )
    })

    output$width_warning <- renderUI({
      grDevices::pdf(NULL) # reset Rplots.pdf for shiny server
      w <- grDevices::dev.size("px")[1]
      grDevices::dev.off()
      if (`if`(!is.null(input$width), input$width, default_slider_width()[1]) < w) {
        helpText(
          icon("triangle-exclamation"),
          "Plot might be cut off for small widths."
        )
      }
    })

    type_download_srv(
      id = "downbutton",
      plot_reactive = plot_r,
      plot_type = plot_type,
      plot_w = reactive(`if`(!is.null(input$width), input$width, default_slider_width()[1])),
      default_w = default_w,
      plot_h = reactive(`if`(!is.null(input$height), input$height, height[1])),
      default_h = default_h
    )

    output$plot_out_modal <- renderUI({
      plotOutput(ns("plot_modal"), height = input$height_in_modal, width = input$width_in_modal)
    })

    observeEvent(input$expand, {
      showModal(
        div(
          class = "plot-modal",
          modalDialog(
            easyClose = TRUE,
            div(
              class = "plot-modal-sliders",
              optionalSliderInputValMinMax(
                inputId = ns("height_in_modal"),
                label = "Plot height",
                value_min_max = round(c(`if`(!is.null(input$height), input$height, height[1]), height[2:3])),
                ticks = FALSE,
                step = 1L,
                round = TRUE
              ),
              optionalSliderInputValMinMax(
                inputId = ns("width_in_modal"),
                label = "Plot width",
                value_min_max = round(c(
                  ifelse(
                    is.null(input$width) || !isFALSE(input$width_resize_switch),
                    ifelse(
                      is.null(input$plot_modal_width) || input$plot_modal_width > default_slider_width()[3],
                      default_slider_width()[1],
                      input$plot_modal_width
                    ),
                    input$width
                  ),
                  default_slider_width()[2:3]
                )),
                ticks = FALSE,
                step = 1L,
                round = TRUE
              )
            ),
            div(
              class = "float-right",
              type_download_ui(ns("modal_downbutton"))
            ),
            div(
              align = "center",
              uiOutput(ns("plot_out_modal"), class = "plot_out_container")
            )
          )
        )
      )
    })

    type_download_srv(
      id = "modal_downbutton",
      plot_reactive = plot_r,
      plot_type = plot_type,
      plot_w = reactive(input$width_in_modal),
      default_w = default_w,
      plot_h = reactive(input$height_in_modal),
      default_h = default_h
    )

    return(
      list(
        brush = reactive({
          # refresh brush data on the main plot size change
          input$height
          input$width
          input$plot_brush
        }),
        click = reactive({
          # refresh click data on the main plot size change
          input$height
          input$width
          input$plot_click
        }),
        dblclick = reactive({
          # refresh double click data on the main plot size change
          input$height
          input$width
          input$plot_dblclick
        }),
        hover = reactive({
          # refresh hover data on the main plot size change
          input$height
          input$width
          input$plot_hover
        }),
        dim = reactive(c(input$width, input$height))
      )
    )
  })
}

#' @keywords internal
type_download_ui <- function(id) {
  ns <- NS(id)
  shinyWidgets::dropdownButton(
    circle = FALSE,
    icon = icon("download"),
    inline = TRUE,
    right = TRUE,
    label = "",
    inputId = ns("downl"),
    div(
      radioButtons(ns("file_format"),
        label = "File type",
        choices = c("png" = "png", "pdf" = "pdf", "svg" = "svg"),
      ),
      textInput(ns("file_name"),
        label = "File name (without extension)",
        value = paste0("plot_", strftime(Sys.time(), format = "%Y%m%d_%H%M%S"))
      ),
      conditionalPanel(
        condition = paste0("input['", ns("file_name"), "'] != ''"),
        downloadButton(ns("data_download"), label = character(0), class = "btn-sm w-full")
      )
    )
  )
}

#' @keywords internal
type_download_srv <- function(id, plot_reactive, plot_type, plot_w, default_w, plot_h, default_h) {
  moduleServer(
    id,
    function(input, output, session) {
      output$data_download <- downloadHandler(
        filename = function() {
          paste(input$file_name, input$file_format, sep = ".")
        },
        content = function(file) {
          width <- `if`(!is.null(plot_w()), plot_w(), default_w())
          height <- `if`(!is.null(plot_h()), plot_h(), default_h())

          # svg and pdf have width in inches and 1 inch = get_plot_dpi() pixels
          switch(input$file_format,
            png = grDevices::png(file, width, height),
            pdf = grDevices::pdf(file, width / get_plot_dpi(), height / get_plot_dpi()),
            svg = grDevices::svg(file, width / get_plot_dpi(), height / get_plot_dpi())
          )

          print_plot(plot_reactive, plot_type)

          grDevices::dev.off()
        }
      )
    }
  )
}

#' Cleans and organizes output to account for NAs and remove empty rows.
#'
#' @description `r lifecycle::badge("stable")`
#' @param data (`data.frame`)\cr
#'  A dataframe from which to select rows.
#' @param brush (`list`)\cr
#'  The data from a brush e.g. input$plot_brush.
#'
#' @return A dataframe of selected rows.
#' @export
#'
clean_brushedPoints <- function(data, brush) { # nolintr
  checkmate::assert_data_frame(data)
  checkmate::assert_list(brush, null.ok = TRUE)

  # define original panelvar1 and panelvar2 before getting overwritten
  original_panelvar1 <- brush$mapping$panelvar1
  original_panelvar2 <- brush$mapping$panelvar2

  # Assign NULL to `mapping$panelvar1` and `mapping$panelvar1` if `brush$panelvar1` and `brush$panelvar1` are NULL
  # This will not evaluate the `panelMatch` step in `brushedPoints` and thus will return a non empty dataframe
  if (is.null(brush$panelvar1)) brush$mapping$panelvar1 <- NULL
  if (is.null(brush$panelvar2)) brush$mapping$panelvar2 <- NULL

  bp_df <- brushedPoints(data, brush)

  # Keep required rows only based on the value of `brush$panelvar1`
  df <- if (is.null(brush$panelvar1) && is.character(original_panelvar1) &&
    is.null(brush$panelvar2) && is.character(original_panelvar2)) {
    df_var1 <- bp_df[is.na(bp_df[[original_panelvar1]]), ]
    df_var1[is.na(df_var1[[original_panelvar2]]), ]
  } else if (is.null(brush$panelvar1) && is.character(original_panelvar1)) {
    bp_df[is.na(bp_df[[original_panelvar1]]), ]
  } else if (is.null(brush$panelvar2) && is.character(original_panelvar2)) {
    bp_df[is.na(bp_df[[original_panelvar2]]), ]
  } else {
    bp_df
  }

  # filter out rows that are only NAs
  df <- df[rowSums(is.na(df)) != ncol(df), ]
  df
}

#' @keywords internal
#'
get_plot_dpi <- function() {
  default_dpi <- 72
  dpi <- getOption("teal.plot_dpi", default_dpi)
  if (!checkmate::test_integerish(dpi, lower = 24, any.missing = FALSE, len = 1)) {
    warning(paste("Invalid value for option 'teal.plot_dpi', therefore defaulting to", default_dpi, "dpi"))
    dpi <- default_dpi
  }
  dpi
}

#' Print plot for download functionality
#'
#' @param plot (`reactive`)\cr
#'  reactive expression to draw a plot
#' @param plot_type (`reactive`)\cr
#'  reactive plot type (`gg`, `trel`, `grob`, `other`)
#'
#' @return Nothing returned, the plot is printed.
#' @keywords internal
#'
print_plot <- function(plot, plot_type) {
  switch(plot_type(),
    "grob" = grid::grid.draw(plot()),
    "other" = {
      graphics::plot.new()
      graphics::text(
        x = graphics::grconvertX(0.5, from = "npc"),
        y = graphics::grconvertY(0.5, from = "npc"),
        labels = "This plot graphic type is not yet supported to download"
      )
    },
    "base" = plot(),
    print(plot())
  )
}

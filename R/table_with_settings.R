#' @keywords internal
#' @noRd
table_with_settings_deps <- function() {
  htmltools::htmlDependency(
    name = "teal-widgets-table-with-settings",
    version = utils::packageVersion("teal.widgets"),
    package = "teal.widgets",
    src = "table-with-settings",
    stylesheet = "table-with-settings.css"
  )
}

#' @name table_with_settings
#'
#' @title `table_with_settings` module
#'
#' @description
#' Module designed to create a `shiny` table output based on `rtable` object (`ElementaryTable` or `TableTree`) input.
#' @inheritParams shiny::moduleServer
#' @param ... (`character`)\cr
#'  Useful for providing additional HTML classes for the output tag.
#'
#' @rdname table_with_settings
#' @export
#'
table_with_settings_ui <- function(id, ...) {
  checkmate::assert_string(id)

  ns <- NS(id)

  tags$div(
    table_with_settings_deps(),
    shinyjs::useShinyjs(),
    bslib::card(
      id = ns("table-with-settings"),
      full_screen = TRUE,
      tags$div(
        class = "teal-widgets settings-buttons",
        bslib::tooltip(
          trigger = tags$div(type_download_ui_table(ns("downbutton"))),
          options = list(trigger = "hover"),
          class = "download-button",
          "Download"
        )
      ),
      tags$div(
        class = "teal-widgets table-content",
        uiOutput(ns("table_out_main"), width = "100%", ...)
      )
    )
  )
}

#' @inheritParams shiny::moduleServer
#' @param table_r (`reactive`)\cr
#'  reactive expression that yields an `rtable` object (`ElementaryTable` or `TableTree`)
#' @param show_hide_signal (`reactive logical`) optional\cr
#'  mechanism to allow modules which call this module to show/hide the table_with_settings UI.
#'
#' @rdname table_with_settings
#'
#' @return A `shiny` module.
#'
#' @export
#'
#' @examples
#' library(shiny)
#' library(rtables)
#' library(magrittr)
#'
#' ui <- bslib::page_fluid(
#'   table_with_settings_ui(
#'     id = "table_with_settings"
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   table_r <- reactive({
#'     l <- basic_table() %>%
#'       split_cols_by("ARM") %>%
#'       analyze(c("SEX", "AGE"))
#'
#'     tbl <- build_table(l, DM)
#'
#'     tbl
#'   })
#'
#'   table_with_settings_srv(id = "table_with_settings", table_r = table_r)
#' }
#'
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }
#'
table_with_settings_srv <- function(id, table_r, show_hide_signal = reactive(TRUE)) {
  checkmate::assert_class(table_r, c("reactive", "function"))
  checkmate::assert_class(show_hide_signal, c("reactive", "function"))

  if (!requireNamespace("rtables", quietly = TRUE)) {
    stop("package rtables is required, please install")
  }

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Turn on and off the UI
    observeEvent(show_hide_signal(), {
      if (show_hide_signal()) {
        shinyjs::show("table-with-settings")
      } else {
        shinyjs::hide("table-with-settings")
      }
    })

    output$table_out_main <- output$table_out_modal <- renderUI({
      rtables::as_html(table_r())
    })

    type_download_srv_table(
      id = "downbutton",
      table_reactive = table_r
    )
  })
}

type_download_ui_table <- function(id) {
  ns <- NS(id)
  bslib::popover(
    icon("download"),
    tags$div(
      radioButtons(ns("file_format"),
        label = "File type",
        choices = c("formatted txt" = ".txt", "csv" = ".csv", "pdf" = ".pdf"),
      ),
      textInput(ns("file_name"),
        label = "File name (without extension)",
        value = paste0("table_", strftime(Sys.time(), format = "%Y%m%d_%H%M%S"))
      ),
      conditionalPanel(
        condition = paste0("input['", ns("file_format"), "'] != '.csv'"),
        tags$div(
          class = "lock-btn",
          title = "on / off",
          shinyWidgets::prettyToggle(
            ns("pagination_switch"),
            value = FALSE,
            label_on = NULL,
            label_off = NULL,
            status_on = "default",
            status_off = "default",
            outline = FALSE,
            plain = TRUE,
            icon_on = icon("fas fa-toggle-off"),
            icon_off = icon("fas fa-toggle-on"),
            animation = "pulse"
          )
        ),
        tags$div(
          class = "paginate-ui",
          shinyWidgets::numericInputIcon(
            inputId = ns("lpp"),
            label = "Paginate table:",
            value = 70,
            icon = list("lines / page")
          ),
          uiOutput(ns("lpp_warning"))
        )
      ),
      conditionalPanel(
        condition = paste0("input['", ns("file_name"), "'] != ''"),
        downloadButton(ns("data_download"), label = character(0), class = "btn-sm w-full")
      )
    )
  )
}

type_download_srv_table <- function(id, table_reactive) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$pagination_switch, {
        if (input$pagination_switch) {
          shinyjs::enable("lpp")
        } else {
          shinyjs::disable("lpp")
        }
      })

      output$lpp_warning <- renderUI({
        catch_warning <- if (input$file_format != ".csv" && input$pagination_switch) {
          try(rtables::paginate_table(
            tt = table_reactive(),
            lpp = as.numeric(input$lpp)
          ), silent = TRUE)
        }

        if (inherits(catch_warning, "try-error")) {
          helpText(
            class = "error",
            icon("triangle-exclamation"),
            "Maximum lines per page includes the reprinted header. Please enter a numeric value or increase the value."
          )
        }
      })

      output$data_download <- downloadHandler(
        filename = function() {
          paste0(input$file_name, input$file_format)
        },
        content = function(file) {
          if (input$file_format == ".txt") {
            rtables::export_as_txt(
              x = table_reactive(),
              file = file,
              paginate = input$pagination_switch,
              lpp = if (input$pagination_switch) as.numeric(input$lpp)
            )
          } else if (input$file_format == ".csv") {
            result <- rtables::matrix_form(table_reactive())$strings
            utils::write.table(
              x = result,
              file = file,
              sep = ",",
              col.names = FALSE,
              row.names = TRUE,
              append = FALSE
            )
          } else {
            rtables::export_as_pdf(
              x = table_reactive(),
              file = file,
              paginate = input$pagination_switch,
              lpp = if (input$pagination_switch) as.numeric(input$lpp)
            )
          }
        }
      )
    }
  )
}

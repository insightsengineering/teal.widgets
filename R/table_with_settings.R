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

#' Render table object to HTML
#'
#' @param x The table object to render
#' @param ... Additional arguments (currently unused)
#' @return HTML representation of the table
#' @keywords internal
#' @noRd
render_table_to_html <- function(x, ...) {
  UseMethod("render_table_to_html", x)
}

#' @method render_table_to_html default
#' @keywords internal
#' @noRd
render_table_to_html.default <- function(x, ...) {
  stop(
    "Unsupported table type. ",
    "table_with_settings supports rtables (ElementaryTable, TableTree), ",
    "gtsummary, or gt (gt_tbl) objects."
  )
}

#' @method render_table_to_html ElementaryTable
#' @keywords internal
#' @noRd
render_table_to_html.ElementaryTable <- function(x, ...) {
  rtables::as_html(x)
}

#' @method render_table_to_html TableTree
#' @keywords internal
#' @noRd
render_table_to_html.TableTree <- function(x, ...) {
  rtables::as_html(x)
}

#' @method render_table_to_html gtsummary
#' @keywords internal
#' @noRd
render_table_to_html.gtsummary <- function(x, ...) {
  gt_obj <- gtsummary::as_gt(x)
  htmltools::HTML(gt::as_raw_html(gt_obj))
}

#' @method render_table_to_html gt_tbl
#' @keywords internal
#' @noRd
render_table_to_html.gt_tbl <- function(x, ...) {
  htmltools::HTML(gt::as_raw_html(x))
}

#' Export table object to file
#'
#' @param x The table object to export
#' @param file The file path to write to
#' @param format The file format (".txt", ".csv", or ".pdf")
#' @param paginate Logical indicating whether to paginate (for rtables)
#' @param lpp Lines per page for pagination (for rtables)
#' @param ... Additional arguments (currently unused)
#' @keywords internal
#' @noRd
export_table <- function(x, file, format, paginate = FALSE, lpp = NULL, ...) {
  UseMethod("export_table", x)
}

#' @method export_table default
#' @keywords internal
#' @noRd
export_table.default <- function(x, file, format, paginate = FALSE, lpp = NULL, ...) {
  stop("Unsupported table type for download")
}

#' @method export_table ElementaryTable
#' @keywords internal
#' @noRd
export_table.ElementaryTable <- function(x, file, format, paginate = FALSE, lpp = NULL, ...) {
  if (format == ".txt") {
    rtables::export_as_txt(
      x = x,
      file = file,
      paginate = paginate,
      lpp = if (paginate) as.numeric(lpp)
    )
  } else if (format == ".csv") {
    result <- rtables::matrix_form(x)$strings
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
      x = x,
      file = file,
      paginate = paginate,
      lpp = if (paginate) as.numeric(lpp)
    )
  }
}

#' @method export_table TableTree
#' @keywords internal
#' @noRd
export_table.TableTree <- function(x, file, format, paginate = FALSE, lpp = NULL, ...) {
  export_table.ElementaryTable(x, file, format, paginate, lpp, ...)
}

#' @method export_table gtsummary
#' @keywords internal
#' @noRd
export_table.gtsummary <- function(x, file, format, paginate = FALSE, lpp = NULL, ...) {
  gt_obj <- gtsummary::as_gt(x)
  # x$data_table can be exported as csv, txt, or pdf
  export_table.gt_tbl(gt_obj, file, format, paginate, lpp, ...)
}

#' @method export_table gt_tbl
#' @keywords internal
#' @noRd
export_table.gt_tbl <- function(x, file, format, paginate = FALSE, lpp = NULL, ...) {
  if (format == ".csv") {
    df_data <- gt::as_raw_html(x) %>%
      rvest::read_html() %>%
      rvest::html_table() %>%
      .[[1]]
    utils::write.csv(df_data, file = file, row.names = FALSE)
  } else if (format == ".pdf") {
    gt::gtsave(x, filename = file)
  } else {
    html_content <- gt::as_raw_html(x)
    writeLines(html_content, file)
  }
}

#' @name table_with_settings
#'
#' @title `table_with_settings` module
#'
#' @description
#' Module designed to create a `shiny` table output based on table objects.
#' Supports `rtables` objects (`ElementaryTable` or `TableTree`), `gtsummary` objects, or `gt` objects.
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
#'  reactive expression that yields a table object. Supported types:
#'  - `rtables` objects (`ElementaryTable` or `TableTree`)
#'  - `gtsummary` objects
#'  - `gt` objects (`gt_tbl`)
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
#' # Example with rtables
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
#' # Example with gtsummary
#' library(shiny)
#' library(gtsummary)
#'
#' ui <- bslib::page_fluid(
#'   table_with_settings_ui(
#'     id = "table_with_settings"
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   table_r <- reactive({
#'     gtsummary::tbl_summary(mtcars)
#'   })
#'
#'   table_with_settings_srv(id = "table_with_settings", table_r = table_r)
#' }
#'
#' if (interactive()) {
#'   shinyApp(ui, server)
#' }
#'
#' # Example with gt
#' library(shiny)
#' library(gt)
#'
#' ui <- bslib::page_fluid(
#'   table_with_settings_ui(
#'     id = "table_with_settings"
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   table_r <- reactive({
#'     mtcars %>%
#'       gt::gt() %>%
#'       gt::tab_header(title = "Motor Trend Car Road Tests")
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
      render_table_to_html(table_r())
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
        table_obj <- table_reactive()
        # Pagination warning only applies to rtables
        if (inherits(table_obj, c("ElementaryTable", "TableTree")) && input$file_format != ".csv" && input$pagination_switch) {
          catch_warning <- try(rtables::paginate_table(
            tt = table_obj,
            lpp = as.numeric(input$lpp)
          ), silent = TRUE)

          if (inherits(catch_warning, "try-error")) {
            helpText(
              class = "error",
              icon("triangle-exclamation"),
              "Maximum lines per page includes the reprinted header. Please enter a numeric value or increase the value."
            )
          }
        }
      })

      output$data_download <- downloadHandler(
        filename = function() {
          paste0(input$file_name, input$file_format)
        },
        content = function(file) {
          export_table(
            x = table_reactive(),
            file = file,
            format = input$file_format,
            paginate = input$pagination_switch,
            lpp = if (input$pagination_switch) as.numeric(input$lpp)
          )
        }
      )
    }
  )
}

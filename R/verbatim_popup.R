#' A `shiny` module that pops up verbatim text.
#' @name verbatim_popup
#' @description `r lifecycle::badge("experimental")`
#' This module consists of a button that once clicked pops up a
#' modal window with verbatim-styled text.
#'
#' @param id (`character(1)`) the `shiny` id
#' @param button_label (`character(1)`) the text printed on the button
#' @param ... additional arguments to `[shiny::actionButton()]`
#'
#' @return the UI function returns a `shiny.tag.list` object
#' @export
#'
#' @examples
#' ui <- shiny::fluidPage(verbatim_popup_ui("my_id", button_label = "Open popup"))
#' srv <- function(input, output) {
#'   verbatim_popup_srv("my_id", "if (TRUE) { print('Popups are the best') }", title = "My custom title")
#' }
#' if (interactive()) shiny::shinyApp(ui, srv)
#'
verbatim_popup_ui <- function(id, button_label, ...) {
  checkmate::assert_string(id)
  checkmate::assert_string(button_label)
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$head(shiny::singleton(
      shiny::includeScript(system.file("js/verbatim_popup.js", package = "teal.widgets"))
    )),
    shiny::actionButton(ns("button"), label = button_label, ...)
  )
}

#' @name verbatim_popup
#' @export
#'
#' @param verbatim_content (`character`, `expression` or `reactive(1)` holding either)
#' the content to show in the popup modal window
#' @param title (`character(1)`) the title of the modal window
#' @param disabled (`reactive(1)`) the `shiny` reactive value holding a `logical`. The popup button is disabled
#' when the flag is `TRUE` and enabled otherwise
#'
verbatim_popup_srv <- function(id, verbatim_content, title, disabled = shiny::reactiveVal(FALSE)) {
  checkmate::assert_string(id)
  checkmate::assert_string(title)
  checkmate::assert_class(disabled, classes = "reactive")
  if (inherits(verbatim_content, "reactive")) {
    shiny::validate(shiny::need(
      checkmate::assert_multi_class(verbatim_content(), classes = c("expression", "character"))
    ))
  } else {
    checkmate::assert_multi_class(verbatim_content, classes = c("expression", "character"))
  }

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    disabled_flag_observer(disabled, ns("button"))
    modal_content <- format_content(verbatim_content)
    button_click_observer(
      shiny::reactive(input$button),
      ns("copy_button"),
      ns("verbatim_content"),
      title,
      modal_content
    )
  })
}

#' Creates a `shiny` observer handling the disabled flag.
#'
#' @details
#' When the flag is `TRUE` the button to open the popup is disabled; it is enabled otherwise.
#'
#' @keywords internal
#' @param disabled_flag (`reactive`) containing the flag
#' @param button_id (`character(1)`) the id of the controlled button
disabled_flag_observer <- function(disabled_flag, button_id) {
  shiny::observeEvent(
    disabled_flag(),
    handlerExpr = {
      if (disabled_flag()) {
        shinyjs::disable(button_id)
      } else {
        shinyjs::enable(button_id)
      }
    }
  )
}

#' Creates a `shiny` observer handling button clicks.
#'
#' @description
#' When the button is clicked it pop up a modal window with the text.
#'
#' @keywords internal
#' @param click_event `reactive` the click event
#' @param copy_button_id (`character(1)`) the id of the button to copy the modal content
#' @param copied_area_id (`character(1)`) the id of the element which contents are copied
#' @param modal_title (`character(1)`) the title of the modal window
#' @param modal_content (`reactive`) the content of the modal window
button_click_observer <- function(click_event, copy_button_id, copied_area_id, modal_title, modal_content) {
  shiny::observeEvent(
    click_event(),
    handlerExpr = {
      shiny::showModal(
        shiny::modalDialog(
        shiny::tagList(
          shiny::tags$div(
            shiny::actionButton(
              copy_button_id,
              "Copy to Clipboard",
              onclick = paste0("copyToClipboard('", copied_area_id, "')")
            ),
            shiny::modalButton("Dismiss"),
            style = "margin-bottom: 15px;"
          ),
          shiny::tags$pre(id = copied_area_id, modal_content()),
        ),
        title = modal_title,
        footer = shiny::tagList(
          shiny::actionButton(
            copy_button_id,
            "Copy to Clipboard",
            onclick = paste0("'copyToClipboard(", copied_area_id, "')")
          ),
          shiny::modalButton("Dismiss")
        ),
        size = "l",
        easyClose = TRUE
      ))
    }
  )
}

#' Formats the content of the modal popup window.
#'
#' @details
#' Formats the content:
#' * concatenates if needed
#' * styles
#'
#' @keywords internal
#' @inheritParams verbatim_popup
#' @return `reactive` with the formatted content
format_content <- function(verbatim_content) {
  shiny::reactive({
    if (inherits(verbatim_content, "reactive")) {
      paste(styler::style_text(paste(as.character(verbatim_content()), collapse = "\n")), collapse = "\n")
    } else {
      paste(styler::style_text(paste(as.character(verbatim_content), collapse = "\n")), collapse = "\n")
    }
  })
}

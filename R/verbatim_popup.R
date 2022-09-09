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
#'   verbatim_popup_srv(
#'     "my_id",
#'     "if (TRUE) { print('Popups are the best') }",
#'     title = "My custom title",
#'     style = TRUE
#'   )
#' }
#' if (interactive()) shiny::shinyApp(ui, srv)
#'
verbatim_popup_ui <- function(id, button_label, ...) {
  checkmate::assert_string(id)
  checkmate::assert_string(button_label)
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(shiny::includeScript(system.file("js/verbatim_popup.js", package = "teal.widgets")))
    ),
    shiny::actionButton(ns("button"), label = button_label, ...)
  )
}

#' @name verbatim_popup
#' @export
#'
#' @param verbatim_content (`character`, `expression` or `reactive(1)` holding either)
#' the content to show in the popup modal window
#' @param title (`character(1)`) the title of the modal window
#' @param style (`logical(1)`) whether to style the `verbatim_content` using `styler::style_text`
#' @param disabled (`reactive(1)`) the `shiny` reactive value holding a `logical`. The popup button is disabled
#' when the flag is `TRUE` and enabled otherwise
#'
verbatim_popup_srv <- function(id, verbatim_content, title, style = FALSE, disabled = shiny::reactiveVal(FALSE)) {
  checkmate::assert_string(id)
  checkmate::assert_string(title)
  checkmate::assert_flag(style)
  checkmate::assert_class(disabled, classes = "reactive")
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    disabled_flag_observer(disabled, "button")
    modal_content <- format_content(verbatim_content, style)
    button_click_observer(
      click_event = shiny::reactive(input$button),
      copy_button_id = ns("copy_button"),
      copied_area_id = ns("verbatim_content"),
      modal_title = title,
      modal_content = modal_content
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
#' @param copy_button_id (`character(1)`) the id of the button to copy the modal content.
#' Automatically appended with a 1 and 2 suffix for top and bottom buttons respectively.
#' @param copied_area_id (`character(1)`) the id of the element which contents are copied
#' @param modal_title (`character(1)`) the title of the modal window
#' @param modal_content (`reactive`) the content of the modal window
button_click_observer <- function(click_event, copy_button_id, copied_area_id, modal_title, modal_content) {
  shiny::observeEvent(
    click_event(),
    handlerExpr = {
      req(modal_content())
      shiny::showModal(
        shiny::modalDialog(
          shiny::tagList(
            include_css_files(pattern = "verbatim_popup"),
            shiny::tags$div(
              class = "mb-4",
              shiny::actionButton(
                paste0(copy_button_id, 1),
                "Copy to Clipboard",
                onclick = paste0("copyToClipboard('", copied_area_id, "')")
              ),
              shiny::modalButton("Dismiss")
            ),
            shiny::tags$pre(id = copied_area_id, modal_content()),
          ),
          title = modal_title,
          footer = shiny::tagList(
            shiny::actionButton(
              paste0(copy_button_id, 2),
              "Copy to Clipboard",
              onclick = paste0("copyToClipboard('", copied_area_id, "')")
            ),
            shiny::modalButton("Dismiss")
          ),
          size = "l",
          easyClose = TRUE
        )
      )
    }
  )
}

#' Formats the content of the modal popup window.
#'
#' @details
#' Formats the content:
#' * concatenates if needed
#' * styles if `style` is TRUE
#'
#' @keywords internal
#' @inheritParams verbatim_popup
#' @return `reactive` with the formatted content
format_content <- function(verbatim_content, style = FALSE) {
  shiny::reactive({
    content <- if (inherits(verbatim_content, "reactive")) {
      verbatim_content()
    } else {
      verbatim_content
    }
    shiny::validate(shiny::need(
      checkmate::test_multi_class(content, classes = c("expression", "character")),
      "verbatim_content should be an expression or a character"
    ))

    content <- paste(as.character(content), collapse = "\n")

    if (style) {
      content <- paste(styler::style_text(content), collapse = "\n")
    }
    content
  })
}
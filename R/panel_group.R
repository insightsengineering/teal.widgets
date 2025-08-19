#' Panel group widget
#'
#' @description `r lifecycle::badge("deprecated")`\cr
#' Designed to group [`panel_item`] elements. Used to handle `shiny` inputs in the encoding panel.
#' @param id optional, (`character`)\cr
#' @param ... (`shiny.tag`)\cr
#'  panels created by [panel_group()]
#'
#' @return (`shiny.tag`)
#'
#' @examples
#'
#' library(shiny)
#' panel_group(
#'   panel_item(
#'     title = "Display",
#'     collapsed = FALSE,
#'     checkboxGroupInput(
#'       "check",
#'       "Tables display",
#'       choices = LETTERS[1:3],
#'       selected = LETTERS[1]
#'     ),
#'     radioButtons(
#'       "radio",
#'       label = "Plot type",
#'       choices = letters[1:2],
#'       selected = letters[1]
#'     )
#'   ),
#'   panel_item(
#'     title = "Pre-processing",
#'     radioButtons(
#'       "filtering",
#'       "What to filter",
#'       choices = LETTERS[1:4],
#'       selected = LETTERS[1]
#'     ),
#'     radioButtons(
#'       "na_action",
#'       "NA action",
#'       choices = letters[1:3],
#'       selected = letters[1]
#'     )
#'   )
#' )
#'
#' @export
panel_group <- function(..., id = NULL) {
  lifecycle::deprecate_soft(
    when = "0.4.3",
    what = "panel_group()",
    details = paste(
      "The `panel_group()` and `panel_item()` view can be achieved by using the `bslib` package.",
      "Please use the `bslib::accordion()` and `bslib::accordion_panel()` functions instead.",
      "This function will be removed in the next release."
    )
  )
  checkmate::assert_string(id, null.ok = TRUE)

  # panel-group
  # div

  bslib::accordion(
    ...
  )
}

#' @keywords internal
#' @noRd
panel_item_deps <- function() {
  htmltools::htmlDependency(
    name = "teal-widgets-panel-item",
    version = utils::packageVersion("teal.widgets"),
    package = "teal.widgets",
    src = "panel-item",
    script = "panel-item.js",
    stylesheet = "panel-item.css"
  )
}

#' Panel item widget
#'
#' @description `r lifecycle::badge("deprecated")`\cr
#' Designed to be grouped using [`panel_group`] element. Used to handle `shiny` inputs in the encoding panel.
#' @param title (`character`)\cr title of panel
#' @param ... content of panel
#' @param collapsed (`logical`) optional,\cr
#'  whether to initially collapse panel
#' @param input_id (`character`) optional\cr
#'  name of the panel item element. If supplied, this will register a shiny input variable that
#'  indicates whether the panel item is open or collapsed and is accessed with `input$input_id`.
#'
#' @return (`shiny.tag`)
#'
#' @examples
#'
#' library(shiny)
#' panel_item(
#'   title = "Display",
#'   collapsed = FALSE,
#'   checkboxGroupInput(
#'     "check",
#'     "Tables display",
#'     choices = LETTERS[1:3],
#'     selected = LETTERS[1]
#'   ),
#'   radioButtons(
#'     "radio",
#'     label = "Plot type",
#'     choices = letters[1:2],
#'     selected = letters[1]
#'   )
#' )
#'
#' @export
panel_item <- function(title, ..., collapsed = TRUE, input_id = NULL) {
  lifecycle::deprecate_soft(
    when = "0.4.3",
    what = "panel_item()",
    details = paste(
      "The `panel_group()` and `panel_item()` view can be achieved by using the `bslib` package.",
      "Please use the `bslib::accordion()` and `bslib::accordion_panel()` functions instead.",
      "This function will be removed in the next release."
    )
  )
  stopifnot(checkmate::test_character(title, len = 1) || inherits(title, c("shiny.tag", "shiny.tag.list", "html")))
  checkmate::assert_flag(collapsed)
  checkmate::assert_string(input_id, null.ok = TRUE)

  bslib::accordion_panel(
    id = input_id,
    title = title,
    open = !collapsed,
    ...
  )
}

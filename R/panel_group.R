#' Panel group widget
#'
#' @description `r lifecycle::badge("experimental")`\cr
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
  checkmate::assert_string(id, null.ok = TRUE)

  # panel-group
  # div

  tags$div(
    id = id,
    ...,
    .renderHook = function(res_tag) {
      bs_version <- get_bs_version()
      if (bs_version == "3") {
        htmltools::tagAppendAttributes(res_tag, class = "panel-group")
      } else if (bs_version %in% c("4", "5")) {
        res_tag <- htmltools::tagAppendAttributes(res_tag, class = "my-4")
        htmltools::tagQuery(res_tag)$
          find(".card")$
          removeClass("my-2")$
          allTags()
      } else {
        stop("Bootstrap 3, 4, and 5 are supported.")
      }
    }
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
#' @description `r lifecycle::badge("experimental")`\cr
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
  stopifnot(checkmate::test_character(title, len = 1) || inherits(title, c("shiny.tag", "shiny.tag.list", "html")))
  checkmate::assert_flag(collapsed)
  checkmate::assert_string(input_id, null.ok = TRUE)

  div_id <- paste0(input_id, "_div")
  panel_id <- paste0(input_id, "_panel_body_", sample(1:10000, 1))


  tags$div(.renderHook = function(res_tag) {
    bs_version <- get_bs_version()

    # alter tag structure
    if (bs_version == "3") {
      res_tag$children <- list(
        tags$div(
          id = div_id,
          class = "panel panel-default",
          tags$div(
            class = paste("panel-heading", ifelse(collapsed, "collapsed", "")),
            `data-toggle` = "collapse",
            href = paste0("#", panel_id),
            `aria-expanded` = ifelse(collapsed, "false", "true"),
            icon(ifelse(collapsed, "angle-right", "angle-down"), class = "dropdown-icon"),
            tags$label(
              class = "panel-title inline",
              title,
            )
          ),
          tags$div(
            class = paste("panel-collapse collapse", ifelse(collapsed, "", "in")),
            id = panel_id,
            tags$div(
              class = "panel-body",
              ...
            )
          )
        )
      )
    } else if (bs_version %in% c("4", "5")) {
      res_tag$children <- list(
        tags$div(
          class = "card my-2",
          tags$div(
            class = "card-header",
            tags$div(
              class = paste("card-heading", ifelse(collapsed, "collapsed", "")),
              # bs4
              `data-toggle` = "collapse",
              # bs5
              `data-bs-toggle` = "collapse",
              href = paste0("#", panel_id),
              `aria-expanded` = ifelse(collapsed, "false", "true"),
              icon(ifelse(collapsed, "angle-right", "angle-down"), class = "dropdown-icon"),
              tags$label(
                class = "card-title inline",
                title,
              )
            )
          ),
          tags$div(
            id = panel_id,
            class = paste("collapse", ifelse(collapsed, "", "show")),
            tags$div(
              class = "card-body",
              ...
            )
          )
        )
      )
    } else {
      stop("Bootstrap 3, 4, and 5 are supported.")
    }


    tagList(
      panel_item_deps(),
      res_tag
    )
  })
}

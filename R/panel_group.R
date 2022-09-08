#' Panel group widget
#'
#' @description `r lifecycle::badge("experimental")`
#' @param id optional, (`character`)\cr
#' @param ... (`shiny.tag`)\cr
#'  panels created by [panel_group()]
#'
#' @return (`shiny.tag`)
#'
#' @export
panel_group <- function(..., id = NULL, class = NULL) {
  checkmate::assert_string(id, null.ok = TRUE)

  # panel-group
  # div

  tags$div(
    id = id,
    class = class,
    class = "panel-group my-4", # only for bs3
    ...
  )
}

#' Panel widget
#' @md
#'
#' @description `r lifecycle::badge("experimental")`
#' @param title (`character`)\cr title of panel
#' @param ... content of panel
#' @param collapsed (`logical`, optional)\cr
#'  whether to initially collapse panel
#' @param input_id (`character`, optional)\cr
#'  name of the panel item element. If supplied, this will register a shiny input variable that
#'  indicates whether the panel item is open or collapsed and is accessed with `input$input_id`.
#'
#' @return (`shiny.tag`)
#'
#' @export
panel_item <- function(title, ..., collapsed = TRUE, input_id = NULL) {
  stopifnot(checkmate::test_character(title, len = 1) || inherits(title, c("shiny.tag", "shiny.tag.list", "html")))
  checkmate::assert_flag(collapsed)
  checkmate::assert_string(input_id, null.ok = TRUE)

  div_id <- paste0(input_id, "_div")
  panel_id <- paste0(input_id, "_panel_body_", sample(1:10000, 1))


  tags$div(.renderHook = function(x) {
    # get theme and version
    theme <- bslib::bs_current_theme()
    version <- if (bslib::is_bs_theme(theme)) {
      bslib::theme_version(theme)
    } else {
      "3"
    }


    # alter tag structure
    if (version == "3") {
      x$children <- list(
        tags$div(
          class = "panel panel-default",
          tags$div(
            id = div_id,
            class = paste("panel-heading", ifelse(collapsed, "collapsed", "")),
            `data-toggle` = "collapse",
            href = paste0("#", panel_id),
            `aria-expanded` = ifelse(collapsed, "false", "true"),
            icon("angle-down", class = "dropdown-icon"),
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
    } else if (version %in% c("4", "5")) {
      x$children <- list(
        tags$div(
          class = "card",
          tags$div(
            class = "card-header",
            tags$div(
              class = ifelse(collapsed, "collapsed", ""),
              `data-toggle` = "collapse",
              `data-bs-toggle` = "collapse",
              href = paste0("#", panel_id),
              `aria-expanded` = ifelse(collapsed, "false", "true"),
              icon("angle-down", class = "dropdown-icon"),
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
    }

    tagList(
      include_css_files(pattern = "panel.css"),
      x
    )
  })
}

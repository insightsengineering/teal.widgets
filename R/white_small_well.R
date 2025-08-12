#' Small well class for HTML
#'
#' @description
#' Adds Small Well class and overflow-x property to HTML output element.
#' @param ... other arguments to pass to tag object's div attributes.
#'
#' @details `white_small_well` is intended to be used with [shiny::uiOutput()].
#' The overflow-x property is set to auto so that a scroll bar is added
#' when the content overflows at the left and right edges of the output window.
#' For example, this is useful for displaying wide tables.
#'
#' @return An HTML output element with class Small Well and overflow-x property
#' @export
#'
#' @examples
#'
#' white_small_well(shiny::htmlOutput("summary"))
white_small_well <- function(...) {
  shiny::tagList(
    tags$div(
      class = "well well-sm",
      style = "background-color: white;",
      ...
    )
  )
}

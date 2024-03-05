#' Standard UI layout
#'
#' @description `r lifecycle::badge("stable")`\cr
#' Create a standard UI layout with output on the right and an encoding panel on
#' the left. This is the layout used by the `teal` modules.
#'
#' @param output (`shiny.tag`)\cr
#'   object with the output element (table, plot, listing) such as for example returned
#' by [shiny::plotOutput()].
#' @param encoding (`shiny.tag`)\cr
#'  object containing the encoding elements. If this element is `NULL` then no encoding side
#'  panel on the right is  created.
#' @param forms (`tagList`)\cr
#'  for example [shiny::actionButton()] that are placed below the encodings panel
#' @param pre_output (`shiny.tag`, optional)\cr
#'  with text placed before the output to put the output into context. For example a title.
#' @param post_output (`shiny.tag`, optional) with text placed after the output to put the output
#' into context. For example the [shiny::helpText()] elements are useful.
#'
#' @return an object of class \code{shiny.tag} with the UI code.
#'
#' @examples
#' library(shiny)
#' standard_layout(
#'   output = white_small_well(h3("Tests")),
#'   encoding = div(
#'     teal.reporter::simple_reporter_ui("simple_reporter"),
#'     tags$label("Encodings", class = "text-primary"),
#'     panel_item(
#'       "Tests",
#'       optionalSelectInput(
#'         "tests",
#'         "Tests:",
#'         choices = c(
#'           "Shapiro-Wilk",
#'           "Kolmogorov-Smirnov (one-sample)"
#'         ),
#'         selected = "Shapiro-Wilk"
#'       )
#'     )
#'   ),
#'   forms = tagList(
#'     verbatim_popup_ui("warning", "Show Warnings"),
#'     verbatim_popup_ui("rcode", "Show R code")
#'   )
#' )
#'
#' @export
standard_layout <- function(output,
                            encoding = NULL,
                            forms = NULL,
                            pre_output = NULL,
                            post_output = NULL) {
  # checking arguments
  checkmate::assert_multi_class(output, c("shiny.tag", "shiny.tag.list", "html"))
  checkmate::assert_multi_class(encoding, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_multi_class(pre_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_multi_class(post_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)

  # if encoding=NULL then forms is placed below output

  tag_output <- div(
    class = "well",
    div(class = "pre-output", pre_output),
    div(class = "output", output),
    div(class = "post-output", post_output)
  )

  tag_enc_out <- if (!is.null(encoding)) {
    tagList(
      div(
        class = "col-md-3",
        div(class = "well", encoding),
        if (is.null(forms)) {
          NULL
        } else {
          div(class = "form-group", forms)
        }
      ),
      div(class = "col-md-9", tag_output)
    )
  } else {
    div(
      class = "col-md-12",
      tag_output,
      if (is.null(forms)) {
        NULL
      } else {
        div(class = "form-group", forms)
      }
    )
  }

  fluidRow(tag_enc_out)
}

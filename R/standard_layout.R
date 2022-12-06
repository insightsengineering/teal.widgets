#' Create a standard UI layout with output on the right and an encoding panel on
#' the left
#'
#' @description `r lifecycle::badge("stable")`
#' This is the layout used for the teal modules in the `beam` package
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
#' @export
standard_layout <- function(output,
                            encoding = NULL,
                            forms = NULL,
                            pre_output = NULL,
                            post_output = NULL) {
  # checking arguments
  if (!inherits(output, c("shiny.tag", "shiny.tag.list", "html"))) {
    stop("output is supposed to be of class shiny.tag or shiny.tag.list")
  }
  for (name in names(l <- list(
    "encoding" = encoding,
    "pre_output" = pre_output,
    "post_output" = post_output
  ))) {
    if (!is.null(l[[name]]) && !inherits(l[[name]], c("shiny.tag", "shiny.tag.list", "html"))) {
      stop(paste(name, "is supposed to be NULL, shiny.tag or shiny.tag.list."))
    }
  }

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

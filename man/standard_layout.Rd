% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/standard_layout.R
\name{standard_layout}
\alias{standard_layout}
\title{Standard UI layout}
\usage{
standard_layout(
  output,
  encoding = NULL,
  forms = NULL,
  pre_output = NULL,
  post_output = NULL
)
}
\arguments{
\item{output}{(\code{shiny.tag})\cr
object with the output element (table, plot, listing) such as for example returned
by \code{\link[shiny:plotOutput]{shiny::plotOutput()}}.}

\item{encoding}{(\code{shiny.tag})\cr
object containing the encoding elements. If this element is \code{NULL} then no encoding side
panel on the right is  created.}

\item{forms}{(\code{tagList})\cr
for example \code{\link[shiny:actionButton]{shiny::actionButton()}} that are placed below the encodings panel}

\item{pre_output}{(\code{shiny.tag}) optional,\cr
with text placed before the output to put the output into context. For example a title.}

\item{post_output}{(\code{shiny.tag}) optional, with text placed after the output to put the output
into context. For example the \code{\link[shiny:helpText]{shiny::helpText()}} elements are useful.}
}
\value{
an object of class \code{shiny.tag} with the UI code.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}\cr
Create a standard UI layout with output on the right and an encoding panel on
the left. This is the layout used by the \code{teal} modules.
}
\examples{
library(shiny)
standard_layout(
  output = white_small_well(tags$h3("Tests")),
  encoding = tags$div(
    tags$label("Encodings", class = "text-primary"),
    panel_item(
      "Tests",
      optionalSelectInput(
        "tests",
        "Tests:",
        choices = c(
          "Shapiro-Wilk",
          "Kolmogorov-Smirnov (one-sample)"
        ),
        selected = "Shapiro-Wilk"
      )
    )
  ),
  forms = tagList(
    verbatim_popup_ui("warning", "Show Warnings"),
    verbatim_popup_ui("rcode", "Show R code")
  )
)

}

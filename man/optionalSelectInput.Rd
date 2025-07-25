% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optionalInput.R
\name{optionalSelectInput}
\alias{optionalSelectInput}
\alias{updateOptionalSelectInput}
\title{Wrapper for \code{pickerInput}}
\usage{
optionalSelectInput(
  inputId,
  label = NULL,
  choices = NULL,
  selected = NULL,
  multiple = FALSE,
  sep = NULL,
  options = list(),
  label_help = NULL,
  fixed = FALSE,
  fixed_on_single = FALSE,
  width = NULL
)

updateOptionalSelectInput(
  session,
  inputId,
  label = NULL,
  selected = NULL,
  choices = NULL
)
}
\arguments{
\item{inputId}{The \code{input} slot that will be used to access the value.}

\item{label}{Display label for the control, or \code{NULL} for no label.}

\item{choices}{List of values to select from. If elements of the
list are named then that name rather than the value is displayed to the user.}

\item{selected}{The initially selected value (or multiple values if \code{multiple = TRUE}).
If not specified then defaults to the first value for single-select lists
and no values for multiple select lists.}

\item{multiple}{Is selection of multiple items allowed?}

\item{sep}{(\code{character(1)})\cr
A separator string to split the \code{choices} or \code{selected} inputs into the values of the different
columns.}

\item{options}{List of options, see \link[shinyWidgets]{pickerOptions} for all available options.
To limit the number of selection possible, see example below.}

\item{label_help}{(\code{shiny.tag}) optional,\cr
e.g. an object returned by \code{\link[shiny:helpText]{shiny::helpText()}}.}

\item{fixed}{(\code{logical(1)}) optional,\cr
whether to block user to select choices.}

\item{fixed_on_single}{(\code{logical(1)}) optional,\cr
whether to block user to select a choice when there is only one or less choice.
When \code{FALSE}, user is still able to select or deselect the choice.}

\item{width}{(\code{character(1)})\cr
The width of the input passed to \code{pickerInput}  e.g. \code{'auto'}, \code{'fit'}, \code{'100px'} or \code{'75\%'}}

\item{session}{(\code{shiny.session})\cr}
}
\value{
(\code{shiny.tag}) HTML tag with \code{pickerInput} widget and
non-interactive element listing selected values.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}
Wrapper for \code{\link[shinyWidgets:pickerInput]{shinyWidgets::pickerInput()}} with additional features.
When \code{fixed = TRUE} or when the number of \code{choices} is less or equal to 1 (see \code{fixed_on_single}),
the \code{pickerInput} widget is hidden and non-interactive widget will be displayed
instead. Toggle of \code{HTML} elements is just the visual effect to avoid displaying
\code{pickerInput} widget when there is only one choice.
}
\examples{
library(shiny)

ui_grid <- function(...) {
  bslib::page_fluid(
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      ...
    )
  )
}

ui <- ui_grid(
  wellPanel(
    optionalSelectInput(
      inputId = "c1",
      label = "Fixed choices",
      choices = LETTERS[1:5],
      selected = c("A", "B"),
      fixed = TRUE
    ),
    verbatimTextOutput(outputId = "c1_out")
  ),
  wellPanel(
    optionalSelectInput(
      inputId = "c2",
      label = "Single choice",
      choices = "A",
      selected = "A"
    ),
    verbatimTextOutput(outputId = "c2_out")
  ),
  wellPanel(
    optionalSelectInput(
      inputId = "c3",
      label = "NULL choices",
      choices = NULL
    ),
    verbatimTextOutput(outputId = "c3_out")
  ),
  wellPanel(
    optionalSelectInput(
      inputId = "c4",
      label = "Default",
      choices = LETTERS[1:5],
      selected = "A"
    ),
    verbatimTextOutput(outputId = "c4_out")
  ),
  wellPanel(
    optionalSelectInput(
      inputId = "c5",
      label = "Named vector",
      choices = c(`A - value A` = "A", `B - value B` = "B", `C - value C` = "C"),
      selected = "A"
    ),
    verbatimTextOutput(outputId = "c5_out")
  ),
  wellPanel(
    selectInput(
      inputId = "c6_choices", label = "Update choices", choices = letters, multiple = TRUE
    ),
    optionalSelectInput(
      inputId = "c6",
      label = "Updated choices",
      choices = NULL,
      multiple = TRUE,
      fixed_on_single = TRUE
    ),
    verbatimTextOutput(outputId = "c6_out")
  )
)

server <- function(input, output, session) {
  observeEvent(input$c6_choices, ignoreNULL = FALSE, {
    updateOptionalSelectInput(
      session = session,
      inputId = "c6",
      choices = input$c6_choices,
      selected = input$c6_choices
    )
  })

  output$c1_out <- renderPrint(input$c1)
  output$c2_out <- renderPrint(input$c2)
  output$c3_out <- renderPrint(input$c3)
  output$c4_out <- renderPrint(input$c4)
  output$c5_out <- renderPrint(input$c5)
  output$c6_out <- renderPrint(input$c6)
}

if (interactive()) {
  shinyApp(ui, server)
}

}

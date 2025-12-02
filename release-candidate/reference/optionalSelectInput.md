# Wrapper for `pickerInput`

Wrapper for
[`shinyWidgets::pickerInput()`](https://dreamrs.github.io/shinyWidgets/reference/pickerInput.html)
with additional features. When `fixed = TRUE` or when the number of
`choices` is less or equal to 1 (see `fixed_on_single`), the
`pickerInput` widget is hidden and non-interactive widget will be
displayed instead. Toggle of `HTML` elements is just the visual effect
to avoid displaying `pickerInput` widget when there is only one choice.

## Usage

``` r
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
```

## Arguments

- inputId:

  The `input` slot that will be used to access the value.

- label:

  Display label for the control, or `NULL` for no label.

- choices:

  List of values to select from. If elements of the list are named then
  that name rather than the value is displayed to the user.

- selected:

  The initially selected value (or multiple values if
  `multiple = TRUE`). If not specified then defaults to the first value
  for single-select lists and no values for multiple select lists.

- multiple:

  Is selection of multiple items allowed?

- sep:

  (`character(1)`)  
  A separator string to split the `choices` or `selected` inputs into
  the values of the different columns.

- options:

  List of options, see
  [pickerOptions](https://dreamrs.github.io/shinyWidgets/reference/pickerOptions.html)
  for all available options. To limit the number of selection possible,
  see example below.

- label_help:

  (`shiny.tag`) optional,  
  e.g. an object returned by
  [`shiny::helpText()`](https://rdrr.io/pkg/shiny/man/helpText.html).

- fixed:

  (`logical(1)`) optional,  
  whether to block user to select choices.

- fixed_on_single:

  (`logical(1)`) optional,  
  whether to block user to select a choice when there is only one or
  less choice. When `FALSE`, user is still able to select or deselect
  the choice.

- width:

  (`character(1)`)  
  The width of the input passed to `pickerInput` e.g. `'auto'`, `'fit'`,
  `'100px'` or `'75%'`

- session:

  (`shiny.session`)  

## Value

(`shiny.tag`) HTML tag with `pickerInput` widget and non-interactive
element listing selected values.

## Examples

``` r
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
```

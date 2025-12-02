# Optional Slider Input Widget

Hidden input widgets are useful to have the `input[[inputId]]` variable
on available in the server function but no corresponding visual clutter
from input widgets that provide only a single choice.

## Usage

``` r
optionalSliderInput(inputId, label, min, max, value, label_help = NULL, ...)
```

## Arguments

- inputId:

  The `input` slot that will be used to access the value.

- label:

  Display label for the control, or `NULL` for no label.

- min, max:

  The minimum and maximum values (inclusive) that can be selected.

- value:

  The initial value of the slider, either a number, a date (class Date),
  or a date-time (class POSIXt). A length one vector will create a
  regular slider; a length two vector will create a double-ended range
  slider. Must lie between `min` and `max`.

- label_help:

  (`shiny.tag`) optional  
  object of class `shiny.tag`, e.g. an object returned by
  [`shiny::helpText()`](https://rdrr.io/pkg/shiny/man/helpText.html)

- ...:

  optional arguments to `sliderInput`

## Value

(`shiny.tag`) HTML tag with `sliderInput` widget.

## Details

if min or max are `NA` then the slider widget will be hidden

## Examples

``` r
ui <- bslib::page_fluid(
  shinyjs::useShinyjs(),
  optionalSliderInput("s", "shown", 0, 1, 0.2),
  optionalSliderInput("h", "hidden", 0, NA, 1),
)
if (interactive()) {
  shiny::shinyApp(ui, function(input, output) {})
}
```

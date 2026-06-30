# A `shiny` module that pops up verbatim text.

This module consists of a button that once clicked pops up a modal
window with verbatim-styled text.

## Usage

``` r
verbatim_popup_ui(id, button_label, type = c("button", "link"), ...)

verbatim_popup_srv(
  id,
  verbatim_content,
  title,
  style = FALSE,
  disabled = shiny::reactiveVal(FALSE)
)
```

## Arguments

- id:

  (`character(1)`) the `shiny` id

- button_label:

  (`character(1)`) the text printed on the button

- type:

  (`character(1)`) specifying whether to use `[shiny::actionButton()]`
  or `[shiny::actionLink()]`.

- ...:

  additional arguments to `[shiny::actionButton()]`(or
  `[shiny::actionLink()]`).

- verbatim_content:

  (`character`, `expression`, `condition` or `reactive(1)` holding any
  of the above) the content to show in the popup modal window

- title:

  (`character(1)`) the title of the modal window

- style:

  (`logical(1)`) whether to style the `verbatim_content` using
  [`styler::style_text`](https://styler.r-lib.org/reference/style_text.html).
  If `verbatim_content` is a `condition` or `reactive` holding
  `condition` then this argument is ignored

- disabled:

  (`reactive(1)`) the `shiny` reactive value holding a `logical`. The
  popup button is disabled when the flag is `TRUE` and enabled
  otherwise.

## Value

the UI function returns a `shiny.tag.list` object

## Examples

``` r
library(shiny)

ui <- bslib::page_fluid(verbatim_popup_ui("my_id", button_label = "Open popup"))
srv <- function(input, output) {
  verbatim_popup_srv(
    "my_id",
    "if (TRUE) { print('Popups are the best') }",
    title = "My custom title",
    style = TRUE
  )
}
if (interactive()) shinyApp(ui, srv)
```

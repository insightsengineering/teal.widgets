# Creates a `shiny` observer handling button clicks.

When the button is clicked it pop up a modal window with the text.

## Usage

``` r
button_click_observer(
  click_event,
  copy_button_id,
  copied_area_id,
  modal_title,
  modal_content,
  disabled
)
```

## Arguments

- click_event:

  `reactive` the click event

- copy_button_id:

  (`character(1)`) the id of the button to copy the modal content.
  Automatically appended with a 1 and 2 suffix for top and bottom
  buttons respectively.

- copied_area_id:

  (`character(1)`) the id of the element which contents are copied

- modal_title:

  (`character(1)`) the title of the modal window

- modal_content:

  (`reactive`) the content of the modal window

- disabled:

  (`reactive(1)`) the `shiny` reactive value holding a `logical`. The
  popup button is disabled when the flag is `TRUE` and enabled
  otherwise.

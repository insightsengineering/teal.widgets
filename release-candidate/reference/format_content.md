# Formats the content of the modal popup window.

Formats the content of the modal popup window.

## Usage

``` r
format_content(verbatim_content, style = FALSE)
```

## Arguments

- verbatim_content:

  (`character`, `expression`, `condition` or `reactive(1)` holding any
  of the above) the content to show in the popup modal window

- style:

  (`logical(1)`) whether to style the `verbatim_content` using
  [`styler::style_text`](https://styler.r-lib.org/reference/style_text.html).
  If `verbatim_content` is a `condition` or `reactive` holding
  `condition` then this argument is ignored

## Value

`reactive` with the formatted content

## Details

Formats the content:

- concatenates if needed

- styles if `style` is TRUE

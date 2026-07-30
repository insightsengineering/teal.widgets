# Small well class for HTML

Adds Small Well class and overflow-x property to HTML output element.

## Usage

``` r
white_small_well(...)
```

## Arguments

- ...:

  other arguments to pass to tag object's div attributes.

## Value

An HTML output element with class Small Well and overflow-x property

## Details

`white_small_well` is intended to be used with
[`shiny::uiOutput()`](https://rdrr.io/pkg/shiny/man/htmlOutput.html).
The overflow-x property is set to auto so that a scroll bar is added
when the content overflows at the left and right edges of the output
window. For example, this is useful for displaying wide tables.

## Examples

``` r

white_small_well(shiny::htmlOutput("summary"))
#> <div class="well well-sm" style="background-color: white;">
#>   <div id="summary" class="shiny-html-output"></div>
#> </div>
```

# Clean brushed points

Cleans and organizes output to account for NAs and remove empty rows.
Wrapper around
[`shiny::brushedPoints`](https://rdrr.io/pkg/shiny/man/brushedPoints.html).

## Usage

``` r
clean_brushedPoints(data, brush)
```

## Arguments

- data:

  (`data.frame`)  
  A data.frame from which to select rows.

- brush:

  (`list`)  
  The data from a brush e.g. `input$plot_brush`.

## Value

A `data.frame` of selected rows.

## Examples

``` r

brush <- list(
  mapping = list(
    x = "AGE",
    y = "BMRKR1"
  ),
  xmin = 30, xmax = 40,
  ymin = 0.7, ymax = 10,
  direction = "xy"
)

data <- data.frame(
  STUDYID = letters[1:20],
  USUBJID = LETTERS[1:20],
  AGE = sample(25:40, size = 20, replace = TRUE),
  BMRKR1 = runif(20, min = 0, max = 12)
)
nrow(clean_brushedPoints(data, brush))
#> [1] 12
data$AGE[1:10] <- NA
nrow(clean_brushedPoints(data, brush))
#> [1] 8
```

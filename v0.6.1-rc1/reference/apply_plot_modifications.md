# This function checks the plot type and applies specific modifications to the plot object based on the provided parameters.

This function checks the plot type and applies specific modifications to
the plot object based on the provided parameters.

## Usage

``` r
apply_plot_modifications(plot_obj, plot_type, dblclicking, ranges)
```

## Arguments

- plot_obj:

  The original plot object.

- plot_type:

  The type of the plot, either `gg` (`ggplot2`) or `grob` (`grid`,
  `graphics`).

- dblclicking:

  A logical value indicating whether double-clicking on data points on
  the main plot is enabled or disabled.

- ranges:

  A list containing x and y values of ranges.

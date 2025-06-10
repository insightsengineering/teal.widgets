#' This function checks the plot type and applies specific modifications
#' to the plot object based on the provided parameters.
#'
#' @param plot_obj The original plot object.
#' @param plot_type The type of the plot, either `gg` (`ggplot2`) or `grob` (`grid`, `graphics`).
#' @param dblclicking A logical value indicating whether double-clicking on data points on
#' the main plot is enabled or disabled.
#' @param ranges A list containing x and y values of ranges.
#'
#' @keywords internal
apply_plot_modifications <- function(plot_obj, plot_type, dblclicking, ranges) {
  if (plot_type == "gg" && dblclicking) {
    plot_obj +
      ggplot2::coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
  } else if (plot_type == "grob") {
    grid::grid.newpage()
    grid::grid.draw(plot_obj)
  } else {
    plot_obj
  }
}

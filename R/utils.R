#' Get bootstrap current version
#' @note will work properly mainly inside a tag `.renderHook`
#' @keywords internal
get_bs_version <- function() {
  theme <- bslib::bs_current_theme()
  if (bslib::is_bs_theme(theme)) {
    bslib::theme_version(theme)
  } else {
    "3"
  }
}

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

#' This function opens a PDF graphics device using \code{\link[grDevices]{pdf}} to suppress
#' the plot display in the IDE. The purpose of this function is to avoid opening graphic devices
#' directly in the IDE.
#'
#' @param x lazy binding which generates the plot(s)
#'
#' @keywords internal
plot_suppress <- function(x) {
  grDevices::pdf(nullfile())
  on.exit(grDevices::dev.off())
  force(x)
}

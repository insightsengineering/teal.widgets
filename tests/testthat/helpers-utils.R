#' Function to check if a function has a side effect of drawing something
#' @param `function` function which possibly draws something.
#' @return `logical(1)` whether the function has a side effect of drawing a plot.
#' @note reference to https://stackoverflow.com/questions/74615694/check-if-a-function-draw-plot-something
#' @keywords internal
is_draw <- function(plot_fun) {
  checkmate::assert_function(plot_fun)
  grDevices::graphics.off() # close any current graphics devices
  cdev <- grDevices::dev.cur()
  grDevices::pdf(nullfile())
  plot_fun()
  if (cdev != grDevices::dev.cur()) {
    on.exit(grDevices::dev.off())
    return(TRUE)
  }
  return(FALSE)
}


is_visible <- function(element, app_driver) {
  any(
    unlist(
      app_driver$get_js(
        sprintf(
          "Array.from(document.querySelectorAll('%s')).map(el => el.checkVisibility())",
          element
        )
      )
    )
  )
}

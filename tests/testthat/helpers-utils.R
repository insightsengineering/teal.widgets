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
    res <- TRUE
  } else {
    res <- FALSE
  }
  res
}

#' Function to check if an element is visible in a shiny app
#'
#' The [shinytest2::AppDriver$wait_for_js()] method used to check if the element
#' throws an error when the element is not visible.
#' Therefore, we use different expectation functions to check for visibility ([testthat::expect_no_error()])
#' and hidden state ([testthat::expect_error()]).
#'
#' @param element `character(1)` CSS selector of the element to check visibility for.
#' @param app_driver `shinytest2::AppDriver` AppDriver object of
#' the shiny app.
#' @param timeout `numeric(1)` maximum time to wait for the element to be
#' visible. The default is the timeout set in the [shinytest2::AppDriver] object.
#' @param expectation_fun `function` expectation function to use for checking
#' visibility.
#' @return `logical(1)` whether the element is visible.
#' @keywords internal
expect_visible <- function(element, app_driver, timeout) {
  tryCatch(
    {
      app_driver$wait_for_js(
        sprintf(
          "Array.from(document.querySelectorAll('%s')).map(el => el.checkVisibility()).some(Boolean)",
          element
        ),
        timeout
      )
      testthat::pass()
    },
    error = function(err) testthat::fail(sprintf("Element '%s' not visible.", element))
  )}

#' @describeIn expect_visible Check if an element is hidden for a given timeout.
expect_hidden <- function(element, app_driver, timeout) {
  tryCatch(
    {
      app_driver$wait_for_js(
        sprintf(
          "!Array.from(document.querySelectorAll('%s')).map(el => el.checkVisibility()).some(Boolean)",
          element
        ),
        timeout
      )
      testthat::pass()
    },
    error = function(err) testthat::fail(sprintf("Element '%s' not visible.", element))
  )
}

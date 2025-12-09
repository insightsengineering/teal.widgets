# Setup timeout options for shinytest2 if none are set in options nor on environment variables
withr::local_options(
  list(
    shinytest2.timeout = getOption(
      "shinytest2.timeout",
      default = Sys.getenv("SHINYTEST2_TIMEOUT", unset = 30 * 1000)
    ),
    shinytest2.load_timeout = getOption(
      "shinytest2.load_timeout",
      default = Sys.getenv("SHINYTEST2_LOAD_TIMEOUT", unset = 60 * 1000)
    ),
    shinytest2.duration = getOption(
      "shinytest2.duration",
      default = Sys.getenv("SHINYTEST2_DURATION", unset = 0.5 * 1000)
    )
  ),
  .local_envir = testthat::test_env()
)

#' Function to check if an element is visible in a shiny app
#'
#' The [shinytest2::AppDriver$wait_for_js()] method is used to check if the element
#' throws an error when the element is not visible.
#' Therefore, we use different expectation functions to check for visibility ([testthat::expect_no_error()])
#' and hidden state ([testthat::expect_error()]).
#'
#' @param selector `character(1)` CSS selector of the element to check visibility for.
#' @param app_driver `shinytest2::AppDriver` AppDriver object of
#' the shiny app.
#' @param timeout `numeric(1)` maximum time to wait for the element to be
#' visible. The default is the timeout set in the [shinytest2::AppDriver] object.
#' @param expectation_fun `function` expectation function to use for checking
#' visibility.
#' @return `logical(1)` whether the element is visible.
#' @keywords internal
expect_visible <- function(selector, app_driver, timeout) {
  checkmate::assert(
    .var.name = "selector",
    combine = "and",
    checkmate::check_string(selector),
    if (grepl("[\"]", selector)) "Cannot contain double quotes (\") in CSS selectors" else TRUE
  )
  checkmate::assert_r6(app_driver, "AppDriver")

  tryCatch(
    {
      app_driver$wait_for_js(
        sprintf(
          "Array.from(document.querySelectorAll(\"%s\")).map(el => el.checkVisibility()).some(Boolean)",
          selector
        ),
        timeout
      )
      testthat::pass()
    },
    error = function(err) {
      testthat::fail(sprintf("CSS selector '%s' does not produce any visible elements.", selector))
    }
  )
}

#' @describeIn expect_visible Check if an element is hidden for a given timeout.
expect_hidden <- function(element, app_driver, timeout) {
  checkmate::assert_string(element)
  checkmate::assert_r6(app_driver, "AppDriver")
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
    error = function(err) testthat::fail(sprintf("Element '%s' is visible.", element))
  )
}

# JS code to click the expand button popup.
click_button_js <- function(selector) {
  checkmate::assert(
    .var.name = "selector",
    combine = "and",
    checkmate::check_string(selector),
    if (grepl("[\"]", selector)) "Cannot contain double quotes (\") in CSS selectors" else TRUE
  )
  sprintf("(btn = document.querySelector(\"%s\")) ? (btn.click(), true) : false;", selector)
}

# JS code to click the download button popup inside the expanded modal.
popover_action_js <- function(selector, action = c("show", "hide")) {
  checkmate::assert(
    .var.name = "selector",
    combine = "and",
    checkmate::check_string(selector),
    if (grepl("[\"]", selector)) "Cannot contain double quotes (\") in CSS selectors" else TRUE
  )
  action <- match.arg(action)
  sprintf("(el = document.querySelector(\"%s\")) ? (bootstrap.Popover.getOrCreateInstance(el).%s(), true) : false;",
   selector,
   action)
}

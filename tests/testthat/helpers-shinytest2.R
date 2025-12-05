# Setup timeout options for shinytest2 if none are set in options nor on environment variables
withr::local_options(
  list(
    shinytest2.timeout = getOption(
      "shinytest2.timeout", default = Sys.getenv("SHINYTEST2_TIMEOUT", unset = 30 * 1000)
    ),
    shinytest2.load_timeout = getOption(
      "shinytest2.load_timeout", default = Sys.getenv("SHINYTEST2_LOAD_TIMEOUT", unset =  60 * 1000)
    ),
    shinytest2.duration = getOption(
      "shinytest2.duration", default = Sys.getenv("SHINYTEST2_DURATION", unset = 0.5 * 1000)
    )
  ),
  .local_envir = testthat::test_env()
)

# JS code to click the expand button popup.
click_button_js <- function(selector) {
  sprintf("(btn = document.querySelector(\"%s\")) ? (btn.click(), true) : false;", selector)
}

# JS code to click the download button popup inside the expanded modal.
popover_action_js <- function(selector, action = c("show", "hide")) {
  action <- match.arg(action)
  js_code <- "
    if (popoverTrigger = document.querySelector(\"%s\")) {
      bootstrap.Popover.getOrCreateInstance(popoverTrigger).%s(); // initialize if doesn't exist
      true;
    } else { false; }
    "
  sprintf(js_code, selector, action)
}

#' Verbatim popup app
#'
#' @description Example table with setting app for testing using \code{shinytest2}
#'
#' @keywords internal
#'
app_driver_vpu <- function(button_label, verbatim_content, title) {
  shiny::shinyApp(
    ui = bslib::page_fluid(
      verbatim_popup_ui(
        id = "verbatim_popup",
        button_label = button_label
      )
    ),
    server = function(input, output, session) {
      verbatim_popup_srv(
        id = "verbatim_popup",
        verbatim_content = verbatim_content,
        title = title,
        style = FALSE
      )
    }
  )
}

testthat::test_that("verbatim_popup_ui returns `shiny.tag.list`", {
  testthat::expect_s3_class(verbatim_popup_ui("STH", "STH2"), "shiny.tag.list")
})

testthat::test_that(
  "e2e: teal.widgets::verbatim_popup is initialized with a button that opens a modal with a verbatim text",
  {
    skip_if_too_deep(5)
    ui_popup_button_label <- "Open me"
    modal_title <- "Verbatim popup title"
    verbatim_content_text <- "if (TRUE) { print('Popups are the best') }"

    app_driver <- shinytest2::AppDriver$new(
      app_driver_vpu(
        button_label = ui_popup_button_label,
        verbatim_content = verbatim_content_text,
        title = modal_title
      ),
      name = "vpu",
      variant = "app_driver_vpu_ui"
    )

    app_driver$wait_for_idle(timeout = default_idle_timeout)

    popup_button_element <- "#verbatim_popup-button"
    testthat::expect_equal(
      app_driver$get_text(popup_button_element),
      ui_popup_button_label
    )

    # Click the button.
    app_driver$click(selector = popup_button_element)
    app_driver$wait_for_idle(timeout = default_idle_timeout)

    # Verify the content of the popped modal is as expected.
    testthat::expect_equal(
      app_driver$get_text(".modal-title"),
      modal_title
    )

    testthat::expect_equal(
      app_driver$get_text("#verbatim_popup-copy_button1"),
      "Copy to Clipboard"
    )
    testthat::expect_equal(
      app_driver$get_text(".modal-footer button:nth-of-type(1)"),
      "Dismiss"
    )

    testthat::expect_equal(
      app_driver$get_text("#verbatim_popup-verbatim_content"),
      verbatim_content_text
    )

    # Modal is closed, once the button is clicked.
    app_driver$click(selector = ".modal-body button[data-dismiss='modal']")
    app_driver$wait_for_idle(timeout = default_idle_timeout)
    testthat::expect_null(app_driver$get_html("#shiny-modal-wrapper"))

    app_driver$stop()
  }
)

testthat::test_that(
  "e2e: verbatim_popup button can be disabled and enabled",
  {
    skip_if_too_deep(5)

    app <- shiny::shinyApp(
      ui = bslib::page_fluid(
        shinyjs::useShinyjs(),
        verbatim_popup_ui(
          id = "verbatim_popup",
          button_label = "Show Popup"
        ),
        shiny::actionButton("toggle_disable", "Toggle Disable")
      ),
      server = function(input, output, session) {
        disabled_state <- shiny::reactiveVal(FALSE)

        verbatim_popup_srv(
          id = "verbatim_popup",
          verbatim_content = "Test content",
          title = "Test Title",
          style = FALSE,
          disabled = disabled_state
        )

        shiny::observeEvent(input$toggle_disable, {
          disabled_state(!disabled_state())
        })
      }
    )

    app_driver <- shinytest2::AppDriver$new(
      app,
      name = "verbatim_popup_disabled",
      variant = "app_driver_vpu_disabled"
    )
    app_driver$wait_for_idle(timeout = default_idle_timeout)

    popup_button_selector <- "#verbatim_popup-button"

    # Initially button should be enabled (not disabled)
    button_html <- app_driver$get_html(popup_button_selector)
    testthat::expect_false(grepl("disabled", button_html, ignore.case = TRUE))

    # Toggle to disabled state
    app_driver$click("toggle_disable")
    app_driver$wait_for_idle(timeout = default_idle_timeout)

    # Button should now be disabled
    button_html <- app_driver$get_html(popup_button_selector)
    testthat::expect_true(grepl("disabled", button_html, ignore.case = TRUE))

    # Toggle back to enabled state
    app_driver$click("toggle_disable")
    app_driver$wait_for_idle(timeout = default_idle_timeout)

    # Button should be enabled again
    button_html <- app_driver$get_html(popup_button_selector)
    testthat::expect_false(grepl("disabled", button_html, ignore.case = TRUE))

    app_driver$stop()
  }
)

testthat::test_that("snapshot test for verbatim_popup_ui", {
  testthat::skip_if_not_installed("withr")
  withr::local_seed(1)
  testthat::expect_snapshot(verbatim_popup_ui("STH", "STH2"))
})

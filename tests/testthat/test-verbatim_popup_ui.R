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

    app <- shinytest2::AppDriver$new(
      app_vpu(
        button_label = ui_popup_button_label,
        verbatim_content = verbatim_content_text,
        title = modal_title
      ),
      name = "pws",
      variant = "app_vpu_ui"
    )

    app$wait_for_idle(timeout = default_idle_timeout)

    popup_button_element <- "#verbatim_popup-button"
    testthat::expect_equal(
      app$get_text(popup_button_element),
      ui_popup_button_label
    )

    # Click the button.
    app$click(selector = popup_button_element)
    app$wait_for_idle(timeout = default_idle_timeout)

    # Verify the content of the popped modal is as expected.
    testthat::expect_equal(
      app$get_text(".modal-title"),
      modal_title
    )

    testthat::expect_equal(
      app$get_text("#verbatim_popup-copy_button1"),
      "Copy to Clipboard"
    )
    testthat::expect_equal(
      app$get_text("#shiny-modal > div > div > div.modal-body > div > button:nth-child(2)"),
      "Dismiss"
    )

    testthat::expect_equal(
      app$get_text("#verbatim_popup-verbatim_content"),
      verbatim_content_text
    )

    # Modal is closed, once the button is clicked.
    app$click(selector = "#shiny-modal-wrapper button[data-dismiss='modal']")
    # There are two Dismiss buttons.
    app$wait_for_idle(timeout = default_idle_timeout)
    testthat::expect_null(app$get_html("#shiny-modal-wrapper"))

    app$stop()
  }
)

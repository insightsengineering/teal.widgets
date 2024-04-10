testthat::test_that("table_with_settings_ui returns `shiny.tag.list`", {
  testthat::expect_s3_class(table_with_settings_ui("STH"), "shiny.tag.list")
})

testthat::test_that("Table with settings: UI screenshots", {
  skip_if_too_deep(5)
  app <- shinytest2::AppDriver$new(
    app_tws(),
    name = "tws",
    variant = "app_tws_ui",
  )
  app$wait_for_idle(timeout = default_idle_timeout)
  app$set_inputs(`table_with_settings-downbutton-file_name` = "table")

  # click on download button
  app$click("table_with_settings-downbutton-dwnl")

  # test clicking on modal
  app$click("table_with_settings-expand")
  # wait for the expand to happen
  Sys.sleep(0.1)
  app$set_inputs(`table_with_settings-modal_downbutton-lpp` = 70)
  app$click("table_with_settings-modal_downbutton-dwnl")
  app$set_inputs(`table_with_settings-modal_downbutton-file_name` = "table")

  # now test values in json
  app$expect_values(screenshot_args = FALSE, name = "final_values")
  app$stop()
})

testthat::test_that(
  "e2e: teal.widgets::table_with_settings is initialized with two buttons and a table",
  {
    skip_if_too_deep(5)
    app <- shinytest2::AppDriver$new(
      app_tws(),
      name = "tws",
      variant = "app_tws_ui",
    )

    app$wait_for_idle(timeout = default_idle_timeout)

    # Check if there are two buttons above the table.
    table_buttons_selector <- "#table_with_settings-table-with-settings > div.table-settings-buttons"
    table_buttons <-
      app$get_html(table_buttons_selector) %>%
      rvest::read_html() %>%
      rvest::html_elements("button")
    testthat::expect_length(table_buttons, 2)
    # Check is the first one is a toggle button.
    testthat::expect_equal(
      table_buttons[[1]] %>%
        rvest::html_attr("data-toggle"),
      "dropdown"
    )
    # First button has specific font-awesome icon.
    dwnl_button <- "#table_with_settings-downbutton-dwnl_state"
    testthat::expect_equal(
      app$get_html(dwnl_button) %>%
        rvest::read_html() %>%
        rvest::html_element("i") %>%
        rvest::html_attr("class"),
      "fas fa-download"
    )

    # Click the first TABLE button.
    app$click(selector = dwnl_button)
    app$wait_for_idle(timeout = default_idle_timeout)

    # Review the content of the toggle.
    testthat::expect_equal(
      app$get_text("#table_with_settings-downbutton-file_format-label"),
      "File type"
    )

    file_format_text <- app$get_text("#table_with_settings-downbutton-file_format > div")
    testthat::expect_match(file_format_text, "formatted txt\n", fixed = TRUE)
    testthat::expect_match(file_format_text, "csv\n", fixed = TRUE)
    testthat::expect_match(file_format_text, "pdf\n", fixed = TRUE)

    testthat::expect_equal(
      app$get_text("#table_with_settings-downbutton-file_name-label"),
      "File name (without extension)"
    )

    testthat::expect_match(
      app$get_value(input = "table_with_settings-downbutton-file_name"),
      sprintf("table_%s", gsub("-", "", Sys.Date()))
    )

    pagination <- "#dropdown-menu-table_with_settings-downbutton-dwnl .paginate-ui .form-group.shiny-input-container"
    pagination_text <- app$get_text(pagination)
    testthat::expect_match(pagination_text, "Paginate table:\n", fixed = TRUE)
    testthat::expect_match(pagination_text, "lines / page\n", fixed = TRUE)

    download_button <- app$get_html("#table_with_settings-downbutton-data_download > i") %>% rvest::read_html()
    testthat::expect_equal(
      download_button %>%
        rvest::html_node("i") %>%
        rvest::html_attr("class"),
      "fas fa-download"
    )
    testthat::expect_equal(
      download_button %>%
        rvest::html_node("i") %>%
        rvest::html_attr("aria-label"),
      "download icon"
    )

    app$click(selector = "input[value='.csv']")
    # check that pagination is missing
    # app$get_text(pagination) # this returns values even though pagination is missing from the view
    testthat::expect_false(
      any(
        unlist(
          app$get_js(
            sprintf(
              "Array.from(document.querySelectorAll('%s')).map(el => el.checkVisibility())",
              pagination_class
            )
          )
        )
      )
    )

    # Click the second TABLE button.
    app$click(selector = "#table_with_settings-expand")
    app$wait_for_idle(timeout = default_idle_timeout)
    # Review the table modal content.

    table_content <- app$get_text("#table_with_settings-table_out_modal")

    check_table <- function(content) {
      testthat::expect_match(content, "B: Placebo", fixed = TRUE)
      testthat::expect_match(content, "C: Combination", fixed = TRUE)
      testthat::expect_match(content, "SEX", fixed = TRUE)
      testthat::expect_match(content, "35.00", fixed = TRUE)
      testthat::expect_match(content, "41.00", fixed = TRUE)
    }
    check_table(table_content)

    # Close modal.
    app$click(selector = "#shiny-modal-wrapper .modal-footer > button")
    app$wait_for_idle(timeout = default_idle_timeout)
    testthat::expect_null(app$get_html("#table_with_settings-table_out_modal"))

    # Review the main table content.
    main_table_content <- app$get_text("#table_with_settings-table_out_main")
    check_table(main_table_content)

    app$stop()
  }
)

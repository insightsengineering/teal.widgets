withr::local_options( # Set longer timeouts for slow tests
  list(
    shinytest2.timeout = 4 * 30 * 1000,
    shinytest2.load_timeout = 4 * 60 * 1000,
    shinytest2.duration = 2 * 0.5 * 1000
  )
)

#' Table with settings app
#'
#' @description Example table with setting app for testing using \code{shinytest2}
#'
#' @keywords internal
#'
app_driver_tws <- function() {
  shiny::shinyApp(
    ui = bslib::page_fluid(
      table_with_settings_ui(
        id = "table_with_settings"
      )
    ),
    server = function(input, output, session) {
      df1 <- data.frame(
        AGE = c(35, 41),
        SEX = factor(c("M", "F")),
        ARM = c("B: Placebo", "C: Combination")
      )

      table_r <- shiny::reactive({
        l1 <- rtables::basic_table()
        l2 <- rtables::split_cols_by(l1, "ARM")
        l3 <- rtables::analyze(l2, c("SEX", "AGE"))
        tbl <- rtables::build_table(l3, df1)
        tbl
      })
      table_with_settings_srv(id = "table_with_settings", table_r = table_r)
    }
  )
}

# JS code to click the expand button popup.
click_expand_popup <- click_button_js(
  "#table_with_settings-table-with-settings > bslib-tooltip > button[aria-label='Expand card']"
)

# JS code to click the download button popup inside the expanded modal.
click_download_popup <- popover_action_js(
  paste(
    "#table_with_settings-table-with-settings .settings-buttons >",
    ".download-button > [data-bs-toggle='tooltip'] bslib-popover > [aria-label='download icon']"
  ),
  action = "show"
)

# JS code to click the download button popup inside the expanded modal.
click_closed_download_popup <- popover_action_js(
  paste(
    "#table_with_settings-table-with-settings .settings-buttons >",
    ".download-button > [data-bs-toggle='tooltip'] bslib-popover > [aria-label='download icon']"
  ),
  action = "hide"
)

check_table <- function(content) {
  testthat::expect_match(content, "B: Placebo", fixed = TRUE, all = FALSE)
  testthat::expect_match(content, "C: Combination", fixed = TRUE, all = FALSE)
  testthat::expect_match(content, "SEX", fixed = TRUE, all = FALSE)
  testthat::expect_match(content, "35.00", fixed = TRUE, all = FALSE)
  testthat::expect_match(content, "41.00", fixed = TRUE, all = FALSE)
}

testthat::test_that("table_with_settings_ui returns `shiny.tag`", {
  testthat::expect_s3_class(table_with_settings_ui("STH"), "shiny.tag")
})

testthat::test_that(
  "e2e: teal.widgets::table_with_settings is initialized with 2 buttons and a table",
  {
    skip_if_too_deep(5)

    app_driver <- shinytest2::AppDriver$new(
      app_driver_tws(),
      name = "tws",
      variant = "app_driver_tws_ui",
      wait = FALSE
    )
    app_driver$wait_for_idle()

    # Check if there is an table.
    expect_visible("#table_with_settings-table_out_main > .rtables-container", app_driver)

    # Check if the settings buttons are visible.
    expect_visible(".teal-widgets.settings-buttons", app_driver)
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e: teal.widgets::table_with_settings: the click on the download button opens a download menu
  with file type, file name and download button",
  {
    testthat::skip_if_not_installed("rvest")
    skip_if_too_deep(5)
    app_driver <- shinytest2::AppDriver$new(
      app_driver_tws(),
      name = "tws",
      variant = "app_driver_tws_ui",
      wait = FALSE
    )
    app_driver$wait_for_idle()

    expect_hidden("#table_with_settings-downbutton-data_download", app_driver)
    expect_hidden("#table_with_settings-downbutton-file_format", app_driver)
    expect_hidden("#table_with_settings-downbutton-file_name", app_driver)

    app_driver$wait_for_js(click_download_popup)
    app_driver$wait_for_idle()

    testthat::expect_equal(
      app_driver$get_text("#table_with_settings-downbutton-file_format-label"),
      "File type"
    )
    testthat::expect_identical(
      app_driver$get_value(input = "table_with_settings-downbutton-file_format"),
      ".txt"
    )

    file_format_text <- app_driver$get_text("#table_with_settings-downbutton-file_format > div")
    testthat::expect_match(file_format_text, "formatted txt\n", fixed = TRUE)
    testthat::expect_match(file_format_text, "csv\n", fixed = TRUE)
    testthat::expect_match(file_format_text, "pdf\n", fixed = TRUE)

    testthat::expect_equal(
      app_driver$get_text("#table_with_settings-downbutton-file_name-label"),
      "File name (without extension)"
    )

    testthat::expect_match(
      app_driver$get_value(input = "table_with_settings-downbutton-file_name"),
      sprintf("table_%s", gsub("-", "", Sys.Date()))
    )

    expect_visible("#table_with_settings-downbutton-data_download > i", app_driver)
    download_button <-
      app_driver$get_html("#table_with_settings-downbutton-data_download > i") %>%
      rvest::read_html()

    testthat::expect_equal(
      download_button %>%
        rvest::html_element("i") %>%
        rvest::html_attr("class"),
      "fas fa-download"
    )
    testthat::expect_equal(
      download_button %>%
        rvest::html_element("i") %>%
        rvest::html_attr("aria-label"),
      "download icon"
    )

    app_driver$stop()
  }
)

testthat::test_that(
  "e2e: teal.widgets::table_with_settings: check pagination appearance for .txt and disappearance for .csv
  for the first button",
  {
    skip_if_too_deep(5)
    app_driver <- shinytest2::AppDriver$new(
      app_driver_tws(),
      name = "tws",
      variant = "app_driver_tws_ui",
      wait = FALSE
    )
    app_driver$wait_for_idle()

    app_driver$wait_for_js(click_download_popup)
    app_driver$wait_for_idle()

    pagination_text <- app_driver$get_text(".paginate-ui")
    testthat::expect_match(pagination_text, "Paginate table:\n", fixed = TRUE)
    testthat::expect_match(pagination_text, "lines / page\n", fixed = TRUE)

    app_driver$click(selector = "input[value='.csv']")
    app_driver$wait_for_idle()

    expect_hidden(".paginate-ui", app_driver)

    app_driver$stop()
  }
)

testthat::test_that(
  "e2e: teal.widgets::table_with_settings: the click on expand button opens a modal with a table",
  {
    skip_if_too_deep(5)
    app_driver <- shinytest2::AppDriver$new(
      app_driver_tws(),
      name = "tws",
      variant = "app_driver_tws_ui",
      wait = FALSE
    )
    app_driver$wait_for_idle()

    expect_hidden("#table_with_settings-table_out_main", app_driver)
    expect_hidden("#bslib-full-screen-overlay", app_driver)

    app_driver$wait_for_js(click_expand_popup)
    app_driver$wait_for_idle()

    expect_visible("#bslib-full-screen-overlay", app_driver)
    table_content <- app_driver$get_text("#table_with_settings-table_out_main .rtables-container")
    check_table(table_content)

    # Close modal.
    app_driver$wait_for_js(click_button_js("#bslib-full-screen-overlay .bslib-full-screen-exit"))
    app_driver$wait_for_idle()

    expect_hidden("#bslib-full-screen-overlay", app_driver)

    # Review the main table content.
    expect_visible("#table_with_settings-table_out_main .rtables-container", app_driver)
    main_table_content <- app_driver$get_text("#table_with_settings-table_out_main")
    check_table(main_table_content)

    app_driver$stop()
  }
)

testthat::test_that(
  "e2e: teal.widgets::table_with_settings: clicking download in an expand modal opens dropdown menu with dwnl settings,
  such as: file type, file name, pagination",
  {
    skip_if_too_deep(5)
    app_driver <- shinytest2::AppDriver$new(
      app_driver_tws(),
      name = "tws",
      variant = "app_driver_tws_ui",
      wait = FALSE
    )
    app_driver$wait_for_idle()

    app_driver$wait_for_js(click_expand_popup)
    app_driver$wait_for_idle()

    app_driver$wait_for_js(click_download_popup)
    app_driver$wait_for_idle()

    testthat::expect_equal(
      app_driver$get_text("#table_with_settings-downbutton-file_format-label"),
      "File type"
    )
    values <- app_driver$get_values()
    testthat::expect_identical(
      values$input$`table_with_settings-downbutton-file_format`,
      ".txt"
    )

    file_format_text <- app_driver$get_text("#table_with_settings-downbutton-file_format > div")
    testthat::expect_match(file_format_text, "formatted txt\n", fixed = TRUE)
    testthat::expect_match(file_format_text, "csv\n", fixed = TRUE)
    testthat::expect_match(file_format_text, "pdf\n", fixed = TRUE)

    testthat::expect_equal(
      app_driver$get_text("#table_with_settings-downbutton-file_name-label"),
      "File name (without extension)"
    )

    testthat::expect_match(
      app_driver$get_value(input = "table_with_settings-downbutton-file_name"),
      sprintf("table_%s", gsub("-", "", Sys.Date()))
    )

    expect_visible("#table_with_settings-downbutton-data_download", app_driver)

    download_button <-
      app_driver$get_html("#table_with_settings-downbutton-data_download > i") %>%
      rvest::read_html()

    testthat::expect_equal(
      download_button %>%
        rvest::html_element("i") %>%
        rvest::html_attr("class"),
      "fas fa-download"
    )
    testthat::expect_equal(
      download_button %>%
        rvest::html_element("i") %>%
        rvest::html_attr("aria-label"),
      "download icon"
    )

    app_driver$stop()
  }
)


testthat::test_that(
  "e2e: teal.widgets::table_with_settings: check pagination appearance for .txt and disappearance for .csv
  for the modal on the second button",
  {
    skip_if_too_deep(5)
    app_driver <- shinytest2::AppDriver$new(
      app_driver_tws(),
      name = "tws",
      variant = "app_driver_tws_ui",
      wait = FALSE
    )
    app_driver$wait_for_idle()

    app_driver$wait_for_js(click_expand_popup)
    app_driver$wait_for_idle()

    app_driver$wait_for_js(click_download_popup)
    app_driver$wait_for_idle()

    pagination_text <- app_driver$get_text(".paginate-ui")
    testthat::expect_match(pagination_text, "Paginate table:\n", fixed = TRUE)
    testthat::expect_match(pagination_text, "lines / page\n", fixed = TRUE)

    app_driver$stop()
  }
)

testthat::test_that(
  "e2e teal.widgets::table_with_settings: clicking download+download button downloads table in a specified format",
  {
    skip_if_too_deep(5)
    app_driver <- shinytest2::AppDriver$new(
      app_driver_tws(),
      name = "tws",
      variant = "app_driver_tws_ui",
      wait = FALSE
    )
    app_driver$wait_for_idle()

    app_driver$wait_for_js(click_download_popup)
    app_driver$wait_for_idle()

    expect_visible("#table_with_settings-downbutton-data_download", app_driver)
    app_driver$wait_for_value(output = "table_with_settings-table_out_main")
    filename <- app_driver$get_download("table_with_settings-downbutton-data_download")
    testthat::expect_match(filename, "txt$", fixed = FALSE)

    content <- readLines(filename)
    check_table(content)

    app_driver$stop()
  }
)

testthat::test_that("e2e teal.widgets::table_with_settings: expanded table can be downloaded", {
  skip_if_too_deep(5)
  app_driver <- shinytest2::AppDriver$new(
    app_driver_tws(),
    name = "tws",
    variant = "app_driver_tws_ui",
    wait = FALSE
  )
  app_driver$wait_for_idle()

  app_driver$wait_for_js(click_expand_popup)
  app_driver$wait_for_idle()

  app_driver$wait_for_js(click_download_popup)
  app_driver$wait_for_idle()

  expect_visible("#table_with_settings-downbutton-data_download", app_driver)
  app_driver$wait_for_value(output = "table_with_settings-downbutton-data_download")
  filename <- app_driver$get_download("table_with_settings-downbutton-data_download")
  testthat::expect_match(filename, "txt$", fixed = FALSE)

  content <- readLines(filename)

  check_table(content)

  app_driver$stop()
})

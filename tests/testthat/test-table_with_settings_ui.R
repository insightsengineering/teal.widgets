#' Table with settings app
#'
#' @description Example table with setting app for testing using \code{shinytest2}
#'
#' @keywords internal
#'
app_driver_tws <- function() {
  shiny::shinyApp(
    ui = shiny::fluidPage(
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

check_table <- function(content) {
  testthat::expect_match(content, "B: Placebo", fixed = TRUE, all = FALSE)
  testthat::expect_match(content, "C: Combination", fixed = TRUE, all = FALSE)
  testthat::expect_match(content, "SEX", fixed = TRUE, all = FALSE)
  testthat::expect_match(content, "35.00", fixed = TRUE, all = FALSE)
  testthat::expect_match(content, "41.00", fixed = TRUE, all = FALSE)
}

testthat::test_that("table_with_settings_ui returns `shiny.tag.list`", {
  testthat::expect_s3_class(table_with_settings_ui("STH"), "shiny.tag.list")
})

testthat::test_that(
  "e2e: teal.widgets::table_with_settings is initialized with 2 buttons and a table",
  {
    skip_if_too_deep(5)
    app_driver <- shinytest2::AppDriver$new(
      app_driver_tws(),
      name = "tws",
      variant = "app_driver_tws_ui",
    )
    app_driver$wait_for_idle(timeout = default_idle_timeout)

    # Check if there is an table.
    testthat::expect_true(is_visible("#table_with_settings-table_out_main", app_driver))

    testthat::expect_true(is_visible("#table_with_settings-downbutton-dwnl", app_driver))
    testthat::expect_true(is_visible("#table_with_settings-expand", app_driver))
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e: teal.widgets::table_with_settings: buttons have proper FA icons and one of them is dropdown",
  {
    skip_if_too_deep(5)
    app_driver <- shinytest2::AppDriver$new(
      app_driver_tws(),
      name = "tws",
      variant = "app_driver_tws_ui",
    )
    app_driver$wait_for_idle(timeout = default_idle_timeout)

    testthat::expect_equal(
      app_driver$get_html("#table_with_settings-downbutton-dwnl") %>%
        rvest::read_html() %>%
        rvest::html_element("button") %>%
        rvest::html_attr("data-toggle"),
      "dropdown"
    )

    testthat::expect_equal(
      app_driver$get_html("#table_with_settings-downbutton-dwnl") %>%
        rvest::read_html() %>%
        rvest::html_element("button") %>%
        rvest::html_attr("aria-expanded"),
      "false"
    )

    testthat::expect_equal(
      app_driver$get_html("#table_with_settings-downbutton-dwnl") %>%
        rvest::read_html() %>%
        rvest::html_element("i") %>%
        rvest::html_attr("class"),
      "fas fa-download"
    )

    testthat::expect_equal(
      app_driver$get_html("#table_with_settings-expand") %>%
        rvest::read_html() %>%
        rvest::html_element("i") %>%
        rvest::html_attr("class"),
      "fas fa-up-right-and-down-left-from-center"
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e: teal.widgets::table_with_settings: the click on the download button opens a download menu
  with file type, file name and download button",
  {
    skip_if_too_deep(5)
    app_driver <- shinytest2::AppDriver$new(
      app_driver_tws(),
      name = "tws",
      variant = "app_driver_tws_ui",
    )
    app_driver$wait_for_idle(timeout = default_idle_timeout)

    testthat::expect_false(is_visible("#table_with_settings-downbutton-data_download", app_driver))
    testthat::expect_false(is_visible("#table_with_settings-downbutton-file_format", app_driver))
    testthat::expect_false(is_visible("#table_with_settings-downbutton-file_name", app_driver))

    app_driver$click(selector = "#table_with_settings-downbutton-dwnl")
    app_driver$wait_for_idle(timeout = default_idle_timeout)

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

    testthat::expect_true(is_visible("#table_with_settings-downbutton-data_download", app_driver))

    download_button <-
      app_driver$get_html("#table_with_settings-downbutton-data_download > i") %>%
      rvest::read_html()

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
    )
    app_driver$wait_for_idle(timeout = default_idle_timeout)

    app_driver$click(selector = "#table_with_settings-downbutton-dwnl")
    app_driver$wait_for_idle(timeout = default_idle_timeout)

    pagination <- "#dropdown-menu-table_with_settings-downbutton-dwnl .paginate-ui .form-group.shiny-input-container"
    pagination_text <- app_driver$get_text(pagination)
    testthat::expect_match(pagination_text, "Paginate table:\n", fixed = TRUE)
    testthat::expect_match(pagination_text, "lines / page\n", fixed = TRUE)

    app_driver$click(selector = "input[value='.csv']")
    app_driver$wait_for_idle(timeout = default_idle_timeout)

    testthat::expect_false(is_visible(pagination, app_driver))

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
    )
    app_driver$wait_for_idle(timeout = default_idle_timeout)

    testthat::expect_false(is_visible("#table_with_settings-table_out_modal", app_driver))

    app_driver$click(selector = "#table_with_settings-expand")
    app_driver$wait_for_idle(timeout = default_idle_timeout)

    table_content <- app_driver$get_text("#table_with_settings-table_out_modal")

    check_table(table_content)

    # Close modal.
    app_driver$click(selector = "#shiny-modal-wrapper .modal-footer > button")
    app_driver$wait_for_idle(timeout = default_idle_timeout)
    testthat::expect_null(app_driver$get_html("#table_with_settings-table_out_modal"))

    # Review the main table content.
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
    )
    app_driver$wait_for_idle(timeout = default_idle_timeout)
    testthat::expect_false(is_visible("#table_with_settings-modal_downbutton-dwnl", app_driver))

    app_driver$click(selector = "#table_with_settings-expand")
    app_driver$wait_for_idle(timeout = default_idle_timeout)
    app_driver$click(selector = "#table_with_settings-modal_downbutton-dwnl")
    app_driver$wait_for_idle(timeout = default_idle_timeout)

    testthat::expect_equal(
      app_driver$get_text("#table_with_settings-modal_downbutton-file_format-label"),
      "File type"
    )
    testthat::expect_identical(
      app_driver$get_value(input = "table_with_settings-modal_downbutton-file_format"),
      ".txt"
    )

    file_format_text <- app_driver$get_text("#table_with_settings-modal_downbutton-file_format > div")
    testthat::expect_match(file_format_text, "formatted txt\n", fixed = TRUE)
    testthat::expect_match(file_format_text, "csv\n", fixed = TRUE)
    testthat::expect_match(file_format_text, "pdf\n", fixed = TRUE)

    testthat::expect_equal(
      app_driver$get_text("#table_with_settings-modal_downbutton-file_name-label"),
      "File name (without extension)"
    )

    testthat::expect_match(
      app_driver$get_value(input = "table_with_settings-modal_downbutton-file_name"),
      sprintf("table_%s", gsub("-", "", Sys.Date()))
    )

    testthat::expect_true(is_visible("#table_with_settings-modal_downbutton-data_download", app_driver))

    download_button <-
      app_driver$get_html("#table_with_settings-modal_downbutton-data_download > i") %>%
      rvest::read_html()

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
    )
    app_driver$wait_for_idle(timeout = default_idle_timeout)
    testthat::expect_false(is_visible("#table_with_settings-modal_downbutton-dwnl", app_driver))

    app_driver$click(selector = "#table_with_settings-expand")
    app_driver$wait_for_idle(timeout = default_idle_timeout)
    app_driver$click(selector = "#table_with_settings-modal_downbutton-dwnl")
    app_driver$wait_for_idle(timeout = default_idle_timeout)

    pagination <-
      "#dropdown-menu-table_with_settings-modal_downbutton-dwnl .paginate-ui .form-group.shiny-input-container"
    pagination_text <- app_driver$get_text(pagination)
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
    )
    app_driver$wait_for_idle(timeout = default_idle_timeout)

    app_driver$click(selector = "#table_with_settings-downbutton-dwnl")
    app_driver$wait_for_idle(timeout = default_idle_timeout)

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
  )
  app_driver$wait_for_idle(timeout = default_idle_timeout)

  app_driver$click(selector = "#table_with_settings-expand")
  app_driver$wait_for_idle(timeout = default_idle_timeout)

  app_driver$click(selector = "#table_with_settings-modal_downbutton-dwnl")
  app_driver$wait_for_idle(timeout = default_idle_timeout)

  filename <- app_driver$get_download("table_with_settings-modal_downbutton-data_download")
  testthat::expect_match(filename, "txt$", fixed = FALSE)

  content <- readLines(filename)

  check_table(content)

  app_driver$stop()
})

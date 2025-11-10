app_driver_osi <- function() {
  ui_grid <- function(...) {
    bslib::page_fluid(
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        ...
      )
    )
  }

  ui <- ui_grid(
    wellPanel(
      optionalSelectInput(
        inputId = "c1",
        label = "Fixed choices",
        choices = LETTERS[1:5],
        selected = c("A", "B"),
        fixed = TRUE
      ),
      verbatimTextOutput(outputId = "c1_out")
    ),
    wellPanel(
      optionalSelectInput(
        inputId = "c2",
        label = "Single choice",
        choices = "A",
        selected = "A"
      ),
      verbatimTextOutput(outputId = "c2_out")
    ),
    wellPanel(
      optionalSelectInput(
        inputId = "c3",
        label = "NULL choices",
        choices = NULL
      ),
      verbatimTextOutput(outputId = "c3_out")
    ),
    wellPanel(
      optionalSelectInput(
        inputId = "c4",
        label = "Default",
        choices = LETTERS[1:5],
        selected = "A"
      ),
      verbatimTextOutput(outputId = "c4_out")
    ),
    wellPanel(
      optionalSelectInput(
        inputId = "c5",
        label = "Named vector",
        choices = c(`A - value A` = "A", `B - value B` = "B", `C - value C` = "C"),
        selected = "A"
      ),
      verbatimTextOutput(outputId = "c5_out")
    ),
    wellPanel(
      selectInput(
        inputId = "c6_choices", label = "Update choices", choices = letters, multiple = TRUE
      ),
      optionalSelectInput(
        inputId = "c6",
        label = "Updated choices",
        choices = NULL,
        multiple = TRUE,
        fixed_on_single = TRUE
      ),
      verbatimTextOutput(outputId = "c6_out")
    )
  )

  server <- function(input, output, session) {
    observeEvent(input$c6_choices, ignoreNULL = FALSE, {
      updateOptionalSelectInput(
        session = session,
        inputId = "c6",
        choices = input$c6_choices,
        selected = input$c6_choices
      )
    })

    output$c1_out <- renderPrint(input$c1)
    output$c2_out <- renderPrint(input$c2)
    output$c3_out <- renderPrint(input$c3)
    output$c4_out <- renderPrint(input$c4)
    output$c5_out <- renderPrint(input$c5)
    output$c6_out <- renderPrint(input$c6)
  }
  shinyApp(ui, server)
}

testthat::test_that(
  "e2e: teal.widgets::optionalSelectInput: initializes",
  {
    skip_if_too_deep(5)
    app_driver <- shinytest2::AppDriver$new(
      app_driver_osi(),
      name = "osi",
      variant = "app_driver_osi_ui"
    )
    app_driver$wait_for_idle(timeout = default_idle_timeout)
    gv <- app_driver$get_values()
    output <- list(
      c1_out = "[1] \"A\" \"B\"", c2_out = "[1] \"A\"", c3_out = "NULL",
      c4_out = "[1] \"A\"", c5_out = "[1] \"A\"", c6_out = "NULL"
    )
    for (i in 1:6) {
      sel <- sprintf(
        "body > div.container-fluid > bslib-layout-columns > div:nth-child(%s)",
        i
      )

      output_sel <- paste0("#c", i, "_out")
      testthat::expect_true(is_visible(sel, app_driver))
      testthat::expect_equal(gv$output[[i]], output[[i]])
    }

    # Check allow clear behavior on c2 input where multiple is FALSE
    allow_clear_selector <- "#c2_input span.bs-select-clear-selected"
    app_driver$click(selector = allow_clear_selector)
    testthat::expect_equal(app_driver$get_value(output = "c2_out"), "NULL")
    testthat::expect_equal(app_driver$get_value(input = "c2"), NULL)
    testthat::expect_equal(
      app_driver$get_text("#c2_input .filter-option-inner-inner"),
      "- Nothing selected -"
    )

    app_driver$stop()
  }
)

testthat::test_that("Check optionalSelectInput is created with different sep", {
  id <- "my_select_input"
  label <- "my_label"
  choices <- c("choice_1", "choice_2")
  sep <- "|=|"
  text_optional_select_input <- as.character(optionalSelectInput(id, label, choices, sep = sep))

  expect_true(
    all(
      c(
        grepl(id, text_optional_select_input),
        grepl(label, text_optional_select_input),
        grepl(sep, text_optional_select_input)
      )
    )
  )
})

testthat::test_that("Check optionalSelectInput is created with label_help", {
  id <- "my_select_input"
  label <- "my_label"
  choices <- c("choice_1", "choice_2")
  label_help <- shiny::helpText("help dummy text")
  text_optional_select_input <- as.character(
    optionalSelectInput(id, label, choices, label_help = label_help)
  )

  expect_true(
    all(
      c(
        grepl(id, text_optional_select_input),
        grepl(label, text_optional_select_input),
        grepl(as.character(label_help), text_optional_select_input)
      )
    )
  )
})

testthat::test_that("Check optionalSelectInput width is applied", {
  id <- "my_select_input"
  label <- "my_label"
  choices <- c("choice_1", "choice_2")
  width <- "200px"
  text_optional_select_input <- as.character(optionalSelectInput(id, label, choices, width = width))

  expect_true(
    all(
      c(
        grepl(id, text_optional_select_input),
        grepl(label, text_optional_select_input),
        grepl(as.character(width), text_optional_select_input)
      )
    )
  )
})


testthat::test_that("Check options for optionalSelectInput are applied", {
  id <- "my_select_input"
  label <- "my_label"
  choices <- c("choice_1", "choice_2")
  options <- list("live-search" = FALSE)
  text_optional_select_input <- as.character(optionalSelectInput(id, label, choices, options = options))

  expect_true(
    all(
      c(
        grepl(id, text_optional_select_input),
        grepl(label, text_optional_select_input),
        grepl("live-search", text_optional_select_input)
      )
    )
  )
})


testthat::test_that("Check that choices are empty if not introduced by user", {
  id <- "my_select_input"
  label <- "my_label"
  expect_true(grepl("option value=\"\"", as.character(optionalSelectInput(id, label))))
})

testthat::test_that("selected marks the initial choice", {
  id <- "my_select_input"
  label <- "my_label"
  choices <- c("choice_1", "choice_2")
  selected <- choices[1]
  text_to_find <- paste0("selected>", selected)
  optional_select_input_text <- as.character(optionalSelectInput(id, label, choices, selected))
  expect_true(grepl(text_to_find, optional_select_input_text))
})

testthat::test_that("there is js show hide functionality only if fixed on single is TRUE and length is more than one", {
  js_on_single_text <- "hide"

  id <- "my_select_input"
  label <- "my_label"
  choices <- c("choice_1", "choice_2")
  optional_select_input_text_not_fixed_on_single <- as.character(
    optionalSelectInput(id, label, choices, fixed_on_single = FALSE)
  )
  optional_select_input_text_fixed_on_single <- as.character(
    optionalSelectInput(id, label, choices, fixed_on_single = TRUE)
  )
  expect_false(grepl("hide", optional_select_input_text_not_fixed_on_single))
  expect_true(grepl("hide", optional_select_input_text_fixed_on_single))
})

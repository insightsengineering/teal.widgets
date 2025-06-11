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
    selector <- sprintf(
      "body > div.container-fluid > bslib-layout-columns > div:nth-child(%s)",
      1:6
    )
    for (sel in selector) {
      testthat::expect_true(is_visible(sel, app_driver))
    }
    app_driver$stop()
  }
)

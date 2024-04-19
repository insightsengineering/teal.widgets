#' Panel group and item
#'
#' @description Example app with a panel on the left for plot settings
#'
#' @keywords internal
#'
app_driver_panel_group <- function() {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      teal.widgets::panel_group(
        teal.widgets::panel_item(
          title = "Plot settings",
          teal.widgets::optionalSliderInputValMinMax("alpha", "Opacity:", c(1, 0, 1), ticks = FALSE),
          teal.widgets::optionalSliderInputValMinMax("size", "Points size:", c(2, 1, 8), ticks = FALSE),
          selectInput(
            inputId = "ggtheme",
            label = "Theme (by ggplot):",
            choices = c(
              "default",
              "gray",
              "bw",
              "linedraw",
              "light",
              "dark",
              "minimal",
              "classic",
              "void",
              "test"
            ),
            selected = "default",
            multiple = FALSE
          )
        )
      )
    ),
    server = function(input, output, session) {

    }
  )
}

testthat::test_that("e2e - panel_group: clicking plot settings accordion panel shows plot setting", {
  skip_if_too_deep(5)

  app_driver <- shinytest2::AppDriver$new(
    app_driver_panel_group(),
    name = "panel_group",
    variant = "app_driver_panel_group",
  )

  app_driver$wait_for_idle(timeout = default_idle_timeout)

  testthat::expect_false(is_visible("#size", app_driver))
  testthat::expect_false(is_visible("#alpha", app_driver))
  testthat::expect_false(is_visible("#ggtheme-label", app_driver))

  # After click they are visible.
  app_driver$click(selector = "#_div > div.panel-heading.collapsed")
  testthat::expect_true(is_visible("#size", app_driver))
  testthat::expect_true(is_visible("#alpha", app_driver))
  testthat::expect_true(is_visible("#ggtheme-label", app_driver))

  app_driver$stop()
})

testthat::test_that("e2e - panel_group: plot settings have default values", {
  skip_if_too_deep(5)

  app_driver <- shinytest2::AppDriver$new(
    app_driver_panel_group(),
    name = "panel_group",
    variant = "app_driver_panel_group",
  )

  app_driver$wait_for_idle(timeout = default_idle_timeout)

  app_driver$click(selector = "#_div > div.panel-heading.collapsed")

  init_vals <- app_driver$get_values()

  testthat::expect_identical(init_vals$input$alpha, 1L)
  testthat::expect_identical(init_vals$input$size, 2L)
  testthat::expect_identical(init_vals$input$ggtheme, "default")

  app_driver$stop()
})

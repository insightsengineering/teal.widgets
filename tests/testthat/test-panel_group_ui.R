testthat::test_that("e2e - panel_group: clicking plot settings accordion panel shows plot setting", {
  skip_if_too_deep(5)

  app_driver <- shinytest2::AppDriver$new(
    app_driver_panel_group(),
    name = "panel_group",
    variant = "app_driver_panel_group",
  )

  app_driver$wait_for_idle(timeout = default_idle_timeout)

  is_visible <- function(element) {
    any(
      unlist(
        app_driver$get_js(
          sprintf(
            "Array.from(document.querySelectorAll('%s')).map(el => el.checkVisibility())",
            element
          )
        )
      )
    )
  }

  testthat::expect_false(is_visible("#size"))
  testthat::expect_false(is_visible("#alpha"))
  testthat::expect_false(is_visible("#ggtheme-label"))

  # After click they are visible.
  app_driver$click(selector = "#_div > div.panel-heading.collapsed")
  testthat::expect_true(is_visible("#size"))
  testthat::expect_true(is_visible("#alpha"))
  testthat::expect_true(is_visible("#ggtheme-label"))

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

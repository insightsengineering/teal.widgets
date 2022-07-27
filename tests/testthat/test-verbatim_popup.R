testthat::test_that("format_content returns a `shiny` reactive when passed a string", {
  test_content <- "Test content"
  testthat::expect_true(inherits(format_content(test_content), "reactive"))
})

testthat::test_that("format_content returns a `shiny` reactive when passed an expression", {
  test_content <- expression("Test content")
  testthat::expect_true(inherits(format_content(test_content), "reactive"))
})

testthat::test_that("format_content returns a `shiny` reactive when passed a reactive", {
  test_content <- shiny::reactiveVal("Test Content")
  testthat::expect_true(inherits(format_content(test_content), "reactive"))
})

testthat::test_that("format_content concatenates the passed array of strings", {
  test_content <- c("Test", "Content")
  result <- shiny::isolate(format_content(test_content))
  testthat::expect_length(result, 1)
})

testthat::test_that("format_content concatenates the passed array of expressions", {
  test_content <- c(expression("test"), expression("content"))
  result <- shiny::isolate(format_content(test_content))
  testthat::expect_length(result, 1)
})

testthat::test_that("format_content concatenates the passed array of string inside a reactive", {
  test_content <- shiny::reactive(c("Test", "Content"))
  result <- shiny::isolate(format_content(test_content)())
  testthat::expect_length(result, 1)
})


testthat::test_that(
  "format_content returns a reactive storing a styled string when passed
  an expression, character or reactive when style = TRUE",
  code = {
    shiny::isolate({
      # Reactive
      test_content <- shiny::reactive(c("Test", "Content"))
      result <- format_content(test_content, style = TRUE)()
      testthat::expect_equal(result, "Test\nContent")

      # Expression
      test_content <- expression({
        print(7)
        if (TRUE) print(8)
      })
      result <- format_content(test_content, style = TRUE)()
      testthat::expect_equal(result, paste(
        "{",
        "  print(7)",
        "  if (TRUE) {",
        "    print(8)",
        "  }",
        "}",
        sep = "\n"
      ))

      # Character
      test_content <- c("Test", "Content")
      result <- format_content(test_content, style = TRUE)()
      testthat::expect_equal(result, "Test\nContent")
    })
  }
)

testthat::test_that("disabled_flag_observer returns an observer", {
  testthat::expect_true(inherits(disabled_flag_observer(shiny::reactive(FALSE), "test_id"), "Observer"))
})

testthat::test_that("button_click_observer returns an observer", {
  testthat::expect_true(inherits(button_click_observer(
    click_event = shiny::reactive(0),
    copy_button_id = "test_copy_button_id",
    copied_area_id = "test",
    modal_title = "test Title",
    modal_content = "Test content"
  ), "Observer"))
})

testthat::test_that("verbatim_popup_ui returns a tag list", {
  testthat::expect_true(
    inherits(verbatim_popup_ui(id = "test_id", button_label = "Test button label"), "shiny.tag.list")
  )
})

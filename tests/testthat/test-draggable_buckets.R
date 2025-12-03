app_driver_db <- function(input_id, label, elements = character(), buckets) {
  ui <- bslib::page_fluid(
    draggable_buckets(
      input_id = input_id,
      label = label,
      elements = elements,
      buckets = buckets
    )
  )
  shiny::shinyApp(ui, function(input, output) {})
}

testthat::test_that(
  "e2e: teal.widgets::draggable_buckets: initializes without input",
  {
    skip_if_too_deep(5)
    app_driver <- shinytest2::AppDriver$new(
      app_driver_db(
        input_id = "id",
        label = "Choices",
        elements = c("a", "b"),
        buckets = c("bucket1", "bucket2")
      ),
      name = "db",
      variant = "app_driver_db_ui",
      wait = FALSE
    )
    app_driver$wait_for_idle()
    testthat::expect_true(is_visible("#id.draggableBuckets.shiny-bound-input", app_driver))
    testthat::expect_identical(
      app_driver$get_value(input = "id"),
      list(
        "bucket1" = list(),
        "bucket2" = list()
      )
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e: teal.widgets::draggable_buckets: initializes with default inputs",
  {
    skip_if_too_deep(5)
    app_driver <- shinytest2::AppDriver$new(
      app_driver_db(
        input_id = "id",
        label = "Choices",
        elements = character(),
        buckets = list(
          "Ref" = "B: Placebo",
          "Comp" = c("A: Drug X", "C: Combination")
        )
      ),
      name = "db",
      variant = "app_driver_db_ui",
      wait = FALSE
    )
    app_driver$wait_for_idle()
    values <- app_driver$get_value(input = "id")
    testthat::expect_identical(
      app_driver$get_value(input = "id"),
      list(
        "Ref" = list("B: Placebo"),
        "Comp" = list("A: Drug X", "C: Combination")
      )
    )
    app_driver$stop()
  }
)

testthat::test_that(
  "e2e: teal.widgets::draggable_buckets: moving elements between buckets updates input",
  {
    skip_if_too_deep(5)
    app_driver <- shinytest2::AppDriver$new(
      app_driver_db(
        input_id = "id",
        label = "Choices",
        elements = character(),
        buckets = list(
          "Ref" = "B: Placebo",
          "Comp" = c("A: Drug X", "C: Combination")
        )
      ),
      name = "db",
      variant = "app_driver_db_ui",
      wait = FALSE
    )
    app_driver$wait_for_idle()
    app_driver$run_js(
      "
      // Find the buckets
      var widget = document.getElementById('id');
      var buckets = widget.querySelectorAll('.bucket');
      var refBucket = Array.from(buckets).find(b => b.dataset.label === 'Ref');
      var compBucket = Array.from(buckets).find(b => b.dataset.label === 'Comp');
      // Find element 'A: Drug X' in compBucket
      var element = Array.from(compBucket.querySelectorAll('.element')).find(e => e.textContent.trim() === 'A: Drug X');
      // Move it to refBucket
      refBucket.appendChild(element);
      "
    )
    app_driver$wait_for_idle()
    testthat::expect_identical(
      app_driver$get_value(input = "id")$Ref,
      list("B: Placebo", "A: Drug X")
    )

    testthat::expect_identical(
      app_driver$get_value(input = "id")$Comp,
      list("C: Combination")
    )
    app_driver$stop()
  }
)

testthat::test_that("fails when inputId is not from the expected type", {
  expect_error(
    draggable_buckets(numeric(1), "test_label", "element_1", "bucket_1"),
    "Assertion on 'input_id' failed: Must be of type 'string', not 'double'."
  )
})

testthat::test_that("fails when label is not from the expected type", {
  expect_error(
    draggable_buckets("my_input_id", numeric(), "element_1", "bucket_1"),
    'Assertion on \'inherits(label, "character") || inherits(label, "shiny.tag")\' failed: Must be TRUE.'
  )
})

testthat::test_that("fails when buckets is not from the expected type", {
  expect_error(draggable_buckets("my_input_id", "test_label", "element_1", numeric()))
})

testthat::test_that("Snapshot test for ui component draggable buckets", {
  my_id <- "my_input_id"
  my_label <- "test_label"
  dummy_element <- "element_1"
  dummy_buckets <- "buckets_1"
  draggable_ui_text <- as.character(draggable_buckets(my_id, my_label, dummy_element, dummy_buckets))

  expect_true(
    all(c(
      grepl(my_id, draggable_ui_text),
      grepl(my_label, draggable_ui_text),
      grepl(dummy_element, draggable_ui_text),
      grepl(dummy_buckets, draggable_ui_text)
    ))
  )
})

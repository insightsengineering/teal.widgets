app_driver_db <- function(input_id, label, elements = character(), buckets) {
  ui <- bslib::page_fluid(
    draggable_buckets(
      input_id = input_id,
      label = label,
      elements = elements,
      buckets = buckets
    )
  )

  shinytest2::AppDriver$new(
    shiny::shinyApp(ui, function(input, output) {})
  )
}

testthat::test_that(
  "e2e: teal.widgets::draggable_buckets: initializes without input",
  {
    skip_if_too_deep(5)
    app_driver <- app_driver_db(
      input_id = "id",
      label = "Choices",
      elements = c("a", "b"),
      buckets = c("bucket1", "bucket2")
    )
    app_driver$wait_for_idle(timeout = default_idle_timeout)
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
    app_driver <- app_driver_db(
      input_id = "id",
      label = "Choices",
      elements = character(),
      buckets = list(
        "Ref" = "B: Placebo",
        "Comp" = c("A: Drug X", "C: Combination")
      )
    )
    app_driver$wait_for_idle(timeout = default_idle_timeout)
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
    app_driver <- app_driver_db(
      input_id = "id",
      label = "Choices",
      elements = character(),
      buckets = list(
        "Ref" = "B: Placebo",
        "Comp" = c("A: Drug X", "C: Combination")
      )
    )
    app_driver$wait_for_idle(timeout = default_idle_timeout)
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
    app_driver$wait_for_idle(timeout = default_idle_timeout)
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

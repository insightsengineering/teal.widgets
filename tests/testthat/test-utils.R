testthat::test_that("get_bs_version", {
  testthat::expect_identical(get_bs_version(), "3")
})

test_that("apply_plot_modifications correctly modifies plot objects", {
  # Create a sample ggplot2 plot object
  ggplot_obj <- ggplot2::ggplot(data = iris, ggplot2::aes(x = Sepal.Length, y = Sepal.Width)) +
    geom_point()

  # Test case 1: Modify ggplot object with dblclicking enabled
  modified_ggplot <- apply_plot_modifications(ggplot_obj, "gg", TRUE, list(x = c(4, 7), y = c(2, 4)))
  expect_true(identical(class(modified_ggplot), class(ggplot_obj)))

  # Test case 2: Modify ggplot object with dblclicking disabled
  modified_ggplot <- apply_plot_modifications(ggplot_obj, "gg", FALSE, list(x = c(4, 7), y = c(2, 4)))
  expect_true(identical(class(modified_ggplot), class(ggplot_obj)))

  # Test case 3: Modify grob object
  grob_obj <- grid::rectGrob()
  modified_grob <- apply_plot_modifications(grob_obj, "grob", TRUE, list(x = c(0, 1), y = c(0, 1)))
  expect_null(modified_grob)

  # Test case 4: Do not modify plot object
  unchanged_plot <- apply_plot_modifications(ggplot_obj, "other_type", TRUE, list(x = c(4, 7), y = c(2, 4)))
  expect_true(identical(unchanged_plot, ggplot_obj))
})

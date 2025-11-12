pkg_name <- "teal.widgets"
library(pkg_name, character.only = TRUE)
if (!is.null(requireNamespace("testthat"))) {
  library(testthat)
  test_check(pkg_name)
} else {
  message("Please install testthat to run tests")
}

pkg_name <- "teal.widgets"
library(pkg_name, character.only = TRUE)
testthat::test_check(pkg_name)
if (!is.null(requireNamespace("testthat"))) library(testthat) else message("Please install testthat to run tests")

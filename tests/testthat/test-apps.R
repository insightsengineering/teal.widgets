
# tests the example apps in ./apps
# each app lives in its own directory, specific to the purpose of the test(s)
# the test specification lives under the app directory

# example:
# apps/pws_ggplot2 tests plot with settings click functionality with a ggplot2
# apps/tws tests table with settings: manipulating and downloading

# nolint start
test_shiny_apps <- function() {
  # skip_on_ci()
  # skip_if_too_deep(5)
  shinytest2::test_app(app_dir = "apps/tws")
}
test_shiny_apps()

# skip_if_too_deep(5)
# skip_on_cran()
# skip_on_ci()
# shinytest2::test_app(app_dir = "apps/tws")

# lapply(
#   list.files("apps", full.names = TRUE),
#   function(shiny_app_dir) {
#     shinytest2::test_app(app_dir = shiny_app_dir)
#   }
# )
# nolint end

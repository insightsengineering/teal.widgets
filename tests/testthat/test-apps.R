

lapply(
  list.files("apps", full.names = TRUE),
  function(shiny_app_dir) {
    shinytest2::test_app(shiny_app_dir)
  }
)

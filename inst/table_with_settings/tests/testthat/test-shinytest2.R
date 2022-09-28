library(shinytest2)

test_that("{shinytest2} recording: table_with_settings_screenshot", {
  app <- AppDriver$new(variant = platform_variant(), name = "table_with_settings_screenshot", 
      height = 823, width = 1459)
  app$expect_screenshot()
})

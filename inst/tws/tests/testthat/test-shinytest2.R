library(shinytest2)

test_that("{shinytest2} recording: tws_screenshot", {
  app <- AppDriver$new(variant = platform_variant(), name = "tws_screenshot",
      height = 823, width = 1459)
  app$expect_screenshot()
})

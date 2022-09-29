library(shinytest2)

test_that("{shinytest2} recording: tws_screenshot", {
  app <- AppDriver$new(variant = platform_variant(), name = "tws_screenshot",
      height = 823, width = 1459)
  app$expect_screenshot()
})

test_that("{shinytest2} recording: tws_download", {
  app <- AppDriver$new(variant = platform_variant(), name = "tws_download", height = 823,
      width = 1459)
  app$set_window_size(width = 1459, height = 766)
  app$set_inputs(`table_with_settings-downbutton-file_format` = ".csv")
  app$set_inputs(`table_with_settings-downbutton-file_format` = ".pdf")
  app$click("table_with_settings-expand")
  app$set_inputs(`table_with_settings-modal_downbutton-lpp` = 70)
  app$set_inputs(`table_with_settings-modal_downbutton-file_format` = ".txt")
  app$set_inputs(`table_with_settings-modal_downbutton-pagination_switch` = FALSE)
  app$expect_screenshot()
})

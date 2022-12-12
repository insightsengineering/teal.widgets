table_r <- reactive({
  l <- rtables::basic_table() %>%
    rtables::split_cols_by("Species") %>%
    rtables::analyze(c("Sepal.Length"))
  rtables::build_table(l, iris)
})

testthat::test_that("table_with_settings_srv: assertions", {
  args <- list(
    list(table_r = "er"),
    list(show_hide_signal = TRUE, table_r = table_r)
  )
  for (arg in args) {
    testthat::expect_error(
      shiny::testServer(
        teal.widgets:::table_with_settings_srv,
        args = arg
      ),
      "Assertion"
    )
  }
})

testthat::test_that("table_with_settings_srv: hide works", {
  testthat::expect_silent(
    shiny::testServer(
      teal.widgets:::table_with_settings_srv,
      args = list(table_r = table_r, show_hide_signal = reactive(FALSE)),
      expr = {}
    )
  )
})

testthat::test_that("table_with_settings_srv: produce plot html", {
  shiny::testServer(
    teal.widgets:::table_with_settings_srv,
    args = list(id = "tws", table_r = table_r),
    expr = {
      expect_type(output$table_out_modal$html, "character")
      expect_equal(output$table_out_main$html, output$table_out_modal$html)
    }
  )
})

testthat::test_that("table_with_settings_srv: expand works", {
  shiny::testServer(
    teal.widgets:::table_with_settings_srv,
    args = list(id = "tws", table_r = table_r),
    expr = {
      session$setInputs(`expand` = TRUE)
      expect_silent(output$table_out_modal$html)
    }
  )
})

testthat::test_that("type_download_srv_table: downloading different output types, custom name", {
  shiny::testServer(
    teal.widgets:::type_download_srv_table,
    args = list(id = "tws", table_reactive = table_r),
    expr = {
      for (down_type in c(".txt", ".csv", ".pdf")){
        session$setInputs(`pagination_switch` = FALSE)
        session$setInputs(`file_name` = "testtable")
        session$setInputs(`file_format` = down_type)
        testthat::expect_true(file.exists(output$data_download))
        testthat::expect_equal(
          basename(output$data_download),
          paste0("testtable", down_type)
          )
      }
    }
  )
})

testthat::test_that("type_download_srv_table: downloading different output types, no name", {
  shiny::testServer(
    teal.widgets:::type_download_srv_table,
    args = list(id = "tws", table_reactive = table_r),
    expr = {
      for (down_type in c(".txt", ".csv", ".pdf")){
        session$setInputs(`pagination_switch` = FALSE)
        session$setInputs(`file_format` = down_type)
        testthat::expect_true(file.exists(output$data_download))
        testthat::expect_equal(
          basename(output$data_download), paste0(down_type)
        )
      }
    }
  )
})

testthat::test_that("type_download_srv_table: downloading different output types, pagination", {
  shiny::testServer(
    teal.widgets:::type_download_srv_table,
    args = list(id = "tws", table_reactive = table_r),
    expr = {
      for (down_type in c(".txt", ".csv", ".pdf")){
        session$setInputs(`pagination_switch` = TRUE)
        session$setInputs(`lpp` = 10)
        session$setInputs(`file_name` = "testtable")
        session$setInputs(`file_format` = down_type)
        testthat::expect_true(file.exists(output$data_download))
        testthat::expect_equal(
          basename(output$data_download),
          paste0("testtable", down_type)
        )
      }
    }
  )
})

testthat::test_that("type_download_srv_table: pagination, lpp to small", {
  testthat::expect_error(
    shiny::testServer(
      teal.widgets:::type_download_srv_table,
      args = list(id = "tws", table_reactive = table_r),
      expr = {
        for (down_type in c(".txt", ".pdf")){
          session$setInputs(`pagination_switch` = TRUE)
          session$setInputs(`lpp` = 1)
          session$setInputs(`file_format` = down_type)
          testthat::expect_true(file.exists(output$data_download))
        }
      }
    ),
    "Lines of repeated context"
  )
})

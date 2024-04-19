#' Table with settings app
#'
#' @description Example table with setting app for testing using \code{shinytest2}
#'
#' @keywords internal
#'
app_driver_tws <- function() {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      table_with_settings_ui(
        id = "table_with_settings"
      )
    ),
    server = function(input, output, session) {
      df1 <- data.frame(
        AGE = c(35, 41),
        SEX = factor(c("M", "F")),
        ARM = c("B: Placebo", "C: Combination")
      )

      table_r <- shiny::reactive({
        l1 <- rtables::basic_table()
        l2 <- rtables::split_cols_by(l1, "ARM")
        l3 <- rtables::analyze(l2, c("SEX", "AGE"))
        tbl <- rtables::build_table(l3, df1)
        tbl
      })
      table_with_settings_srv(id = "table_with_settings", table_r = table_r)
    }
  )
}


#' Panel group and item
#'
#' @description Example app with a panel on the left for plot settings
#'
#' @keywords internal
#'
app_driver_panel_group <- function() {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      teal.widgets::panel_group(
        teal.widgets::panel_item(
          title = "Plot settings",
          teal.widgets::optionalSliderInputValMinMax("alpha", "Opacity:", c(1, 0, 1), ticks = FALSE),
          teal.widgets::optionalSliderInputValMinMax("size", "Points size:", c(2, 1, 8), ticks = FALSE),
          selectInput(
            inputId = "ggtheme",
            label = "Theme (by ggplot):",
            choices = c(
              "default",
              "gray",
              "bw",
              "linedraw",
              "light",
              "dark",
              "minimal",
              "classic",
              "void",
              "test"
            ),
            selected = "default",
            multiple = FALSE
          )
        )
      )
    ),
    server = function(input, output, session) {

    }
  )
}
#' Verbatim popup app
#'
#' @description Example table with setting app for testing using \code{shinytest2}
#'
#' @keywords internal
#'
app_driver_vpu <- function(button_label, verbatim_content, title) {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      verbatim_popup_ui(
        id = "verbatim_popup",
        button_label = button_label
      )
    ),
    server = function(input, output, session) {
      verbatim_popup_srv(
        id = "verbatim_popup",
        verbatim_content = verbatim_content,
        title = title,
        style = FALSE
      )
    }
  )
}

#' Function to check if a function has a side effect of drawing something
#' @param `function` function which possibly draws something.
#' @return `logical(1)` whether the function has a side effect of drawing a plot.
#' @note reference to https://stackoverflow.com/questions/74615694/check-if-a-function-draw-plot-something
#' @keywords internal
is_draw <- function(plot_fun) {
  checkmate::assert_function(plot_fun)
  grDevices::graphics.off() # close any current graphics devices
  cdev <- grDevices::dev.cur()
  plot_fun()
  if (cdev != grDevices::dev.cur()) {
    on.exit(grDevices::dev.off())
    return(TRUE)
  }
  return(FALSE)
}


is_visible <- function(element, app_driver) {
  any(
    unlist(
      app_driver$get_js(
        sprintf(
          "Array.from(document.querySelectorAll('%s')).map(el => el.checkVisibility())",
          element
        )
      )
    )
  )
}

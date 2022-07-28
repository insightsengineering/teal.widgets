#' Panel group widget
#'
#' @description `r lifecycle::badge("stable")`
#' @param input_id optional, (`character`)\cr
#'  name of the panel group element. If supplied, this will register a shiny input variable that
#'  indicates which panel item is open (accessed with `input$input_id`) and will collapse all other
#'  panel items when one is open.
#' @param ... (`shiny.tag`)\cr
#'  panels created by [panel_group()]
#'
#' @return (`shiny.tag`)
#'
#' @export
panel_group <- function(..., input_id = NULL) {
  checkmate::assert_string(input_id, null.ok = TRUE)

  tags$div(
    id = input_id,
    class = "panel-group",
    shinyjs::useShinyjs(),
    # allow input to be accessed on initialization without !is.null
    # if server code runs before input initialized with JS (not teal)
    if (!is.null(input_id)) shinyjs::hidden(tags$input(id = input_id, type = "text", value = "None")),
    if (!is.null(input_id)) {
      tags$head(tags$script(accordion(input_id)))
    },
    ...
  )
}

#' Panel widget
#' @md
#'
#' @description `r lifecycle::badge("stable")`
#' @param title (`character`)\cr title of panel
#' @param ... content of panel
#' @param collapsed (`logical`, optional)\cr
#'  whether to initially collapse panel
#' @param input_id (`character`, optional)\cr
#'  name of the panel item element. If supplied, this will register a shiny input variable that
#'  indicates whether the panel item is open or collapsed and is accessed with `input$input_id`.
#'
#' @return (`shiny.tag`)
#'
#' @export
panel_item <- function(title, ..., collapsed = TRUE, input_id = NULL) {
  stopifnot(checkmate::test_character(title, len = 1) || inherits(title, c("shiny.tag", "shiny.tag.list", "html")))
  checkmate::assert_flag(collapsed)
  checkmate::assert_string(input_id, null.ok = TRUE)

  div_id <- paste0(input_id, "_div")
  panel_id <- paste0(input_id, "_panel_body_", sample(1:10000, 1))

  tagList(
    tags$head(tags$script(if (!is.null(input_id)) panel_status(input_id, div_id, panel_id))),
    include_css_files(pattern = "panel.css"),
    shinyjs::useShinyjs(),
    # allow input to be accessed on initialization without !is.null
    # if server code runs before input initialized with JS (not teal)
    if (!is.null(input_id)) shinyjs::hidden(tags$input(id = input_id, type = "checkbox", value = collapsed)),
    tags$div(
      class = "panel panel-default",
      tags$div(
        id = div_id,
        class = paste("panel-heading", ifelse(collapsed, "collapsed", "")),
        `data-toggle` = "collapse",
        href = paste0("#", panel_id),
        `aria-expanded` = ifelse(collapsed, "false", "true"),
        icon("angle-down", class = "dropdown-icon"),
        tags$label(
          class = "panel-title inline",
          title,
        )
      ),
      tags$div(
        class = paste("panel-collapse collapse", ifelse(collapsed, "", "in")),
        id = panel_id,
        tags$div(
          class = "panel-body",
          ...
        )
      )
    )
  )
}


#' Get Collapsible Panel Status
#'
#' Javascript to get open or close status of a panel item.
#' @md
#'
#' @param input_id (`character`)\cr
#'  string with which to register a shiny input. The title of the currently open panel (or last
#'  opened panel) will be accessible with `input$input_id`.
#' @param div_id (`character`)\cr
#'  string giving the id of the `div` to listen for collapse events on.
#' @param panel_id (`character`)\cr
#'  string giving the id of the panel to determine the initial collapsed status.
#' @keywords internal
panel_status <- function(input_id, div_id, panel_id) {
  paste0("
    $(document).ready(function(event) { // wait for all HTML elements to be loaded
      var panel_item = document.querySelector('#", div_id, "');

      var observer = new MutationObserver(function(mutations) {
        mutations.forEach(function(mutation) {
          if (mutation.type == 'attributes') {
            // is the element collapsed or not
            var aria = document.getElementById('", div_id, "').getAttribute('aria-expanded');
            var collapsed = (aria == 'false'); // convert to boolean
            Shiny.onInputChange('", input_id, "', collapsed); // update shiny input
          }
        });
      });

      observer.observe(panel_item, {
        attributes: true // listen for attribute changes
      });

      // set initial value
      var aria = document.getElementById('", panel_id, "').classList.contains('in');
      var collapsed = (aria == false);
      Shiny.onInputChange('", input_id, "', collapsed);
    });
  ")
}

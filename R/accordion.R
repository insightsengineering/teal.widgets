#' Accordion Script
#'
#' Javascript to make a panel group into an accordion where
#' only one panel item can be open at once.
#' @md
#'
#' @param input_id (`character(1)`)\cr
#'  string with which to register a shiny input. The title of the currently open panel (or
#'  last opened panel) will be accessible with `input$input_id`.
#' @keywords internal
accordion <- function(input_id) {
  paste0("
    $(document).ready(function(event) { // wait for all HTML elements to be loaded

      // create accordion by defining data-parent attribute
      $('#", input_id, "')
      .children('.panel')
      .children('.panel-heading')
      .attr('data-parent', '#", input_id, "');

      // if more than one panel is open at start
      // close all except the first
      if (
        parseInt($('#", input_id, "')
        .children('.panel')
        .children('.panel-collapse.in')
        .length) != 1
        ) {
          // open the first
          $('#", input_id, "')
          .children('.panel')
          .children('.panel-collapse')
          .first()
          .addClass('in');

          // close the rest
          $('#", input_id, "')
          .children('.panel')
          .children('.panel-collapse.in')
          .slice(1)
          .removeClass('in');
          };

      var panel = $('#", input_id, "')
      .children('.panel')
      .children('.panel-collapse.in')
      .parents('.panel-default')
      .children('.panel-heading')
      .text()
      .trim();

      Shiny.onInputChange('", input_id, "', panel) // initialize which panel is open

      // when a panel is shown, update input with which panel triggered the event
      $('#", input_id, "')
      .children('.panel')
      .on('shown.bs.collapse', function (e) {
        var panel = $(e.target).parents('.panel-default').children('.panel-heading').text().trim();
        Shiny.onInputChange('", input_id, "', panel) // which panel is open
      });
    });
  ")
}

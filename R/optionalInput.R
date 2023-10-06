#' Wrapper for `pickerInput`
#'
#' @description `r lifecycle::badge("stable")`
#' Wrapper for [shinyWidgets::pickerInput()] with additional features.
#' When `fixed = TRUE` or when the number of `choices` is less or equal to 1 (see `fixed_on_single`),
#' the `pickerInput` widget is hidden and non-interactive widget will be displayed
#' instead. Toggle of `HTML` elements is just the visual effect to avoid displaying
#' `pickerInput` widget when there is only one choice.
#'
#' @inheritParams shinyWidgets::pickerInput
#'
#' @param sep (`character(1)`)\cr
#'  A separator string to split the `choices` or `selected` inputs into the values of the different
#'  columns.
#'
#' @param label_help (`shiny.tag` optional)\cr
#'  e.g. an object returned by [shiny::helpText()].
#'
#' @param fixed (`logical(1)` optional)\cr
#'  whether to block user to select choices.
#'
#' @param fixed_on_single (`logical(1)` optional)\cr
#'  whether to block user to select a choice when there is only one or less choice.
#'  When `FALSE`, user is still able to select or deselect the choice.
#'
#' @param width (`character(1)`)\cr
#'  The width of the input passed to `pickerInput`  e.g. `'auto'`, `'fit'`, `'100px'` or `'75%'`
#'
#' @return (`shiny.tag`) HTML tag with `pickerInput` widget and
#'  non-interactive element listing selected values.
#'
#' @export
#'
#' @examples
#' library(shiny)
#'
#' # Create a minimal example data frame
#' data <- data.frame(
#'   AGE = c(25, 30, 40, 35, 28),
#'   SEX = c("Male", "Female", "Male", "Female", "Male"),
#'   PARAMCD = c("Val1", "Val2", "Val3", "Val4", "Val5"),
#'   PARAM = c("Param1", "Param2", "Param3", "Param4", "Param5"),
#'   AVISIT = c("Visit1", "Visit2", "Visit3", "Visit4", "Visit5"),
#'   stringsAsFactors = TRUE
#' )
#'
#' ui_grid <- function(...) {
#'   fluidPage(
#'     fluidRow(
#'       lapply(list(...), function(x) column(4, wellPanel(x)))
#'     )
#'   )
#' }
#'
#'
#' app <- shinyApp(
#'   ui = ui_grid(
#'     div(
#'       optionalSelectInput(
#'         inputId = "c1",
#'         label = "Fixed choices",
#'         choices = LETTERS[1:5],
#'         selected = c("A", "B"),
#'         fixed = TRUE
#'       ),
#'       verbatimTextOutput(outputId = "c1_out")
#'     ),
#'     div(
#'       optionalSelectInput(
#'         inputId = "c2",
#'         label = "Single choice",
#'         choices = "A",
#'         selected = "A"
#'       ),
#'       verbatimTextOutput(outputId = "c2_out")
#'     ),
#'     div(
#'       optionalSelectInput(
#'         inputId = "c3",
#'         label = "NULL choices",
#'         choices = NULL
#'       ),
#'       verbatimTextOutput(outputId = "c3_out")
#'     ),
#'     div(
#'       optionalSelectInput(
#'         inputId = "c4",
#'         label = "Default",
#'         choices = LETTERS[1:5],
#'         selected = "A"
#'       ),
#'       verbatimTextOutput(outputId = "c4_out")
#'     ),
#'     div(
#'       optionalSelectInput(
#'         inputId = "c5",
#'         label = "Named vector",
#'         choices = c(`A - value A` = "A", `B - value B` = "B", `C - value C` = "C"),
#'         selected = "A"
#'       ),
#'       verbatimTextOutput(outputId = "c5_out")
#'     ),
#'     div(
#'       selectInput(
#'         inputId = "c6_choices", label = "Update choices", choices = letters, multiple = TRUE
#'       ),
#'       optionalSelectInput(
#'         inputId = "c6",
#'         label = "Updated choices",
#'         choices = NULL,
#'         multiple = TRUE,
#'         fixed_on_single = TRUE
#'       ),
#'       verbatimTextOutput(outputId = "c6_out")
#'     )
#'   ),
#'   server = function(input, output, session) {
#'     observeEvent(input$c6_choices, ignoreNULL = FALSE, {
#'       updateOptionalSelectInput(
#'         session = session,
#'         inputId = "c6",
#'         choices = input$c6_choices,
#'         selected = input$c6_choices
#'       )
#'     })
#'
#'     output$c1_out <- renderPrint({
#'       input$c1
#'     })
#'     output$c2_out <- renderPrint({
#'       input$c2
#'     })
#'     output$c3_out <- renderPrint({
#'       input$c3
#'     })
#'     output$c4_out <- renderPrint({
#'       input$c4
#'     })
#'     output$c5_out <- renderPrint({
#'       input$c5
#'     })
#'     output$c6_out <- renderPrint({
#'       input$c6
#'     })
#'   }
#' )
#'
#' if (interactive()) {
#'   runApp(app)
#' }
#'
optionalSelectInput <- function(inputId, # nolint
                                label = NULL,
                                choices = NULL,
                                selected = NULL,
                                multiple = FALSE,
                                sep = NULL,
                                options = list(),
                                label_help = NULL,
                                fixed = FALSE,
                                fixed_on_single = FALSE,
                                width = NULL) {
  checkmate::assert_string(inputId)
  checkmate::assert(
    checkmate::check_string(label, null.ok = TRUE),
    checkmate::check_class(label, "shiny.tag"),
    checkmate::check_class(label, "shiny.tag.list"),
    checkmate::check_class(label, "html")
  )
  stopifnot(is.null(choices) || length(choices) >= 1)
  stopifnot(
    is.null(selected) ||
      length(selected) == 0 ||
      all(selected %in% choices) ||
      all(selected %in% unlist(choices, recursive = FALSE))
  )
  checkmate::assert_flag(multiple)
  checkmate::assert_string(sep, null.ok = TRUE)
  checkmate::assert_list(options)
  checkmate::assert(
    checkmate::check_string(label_help, null.ok = TRUE),
    checkmate::check_class(label_help, "shiny.tag"),
    checkmate::check_class(label_help, "shiny.tag.list"),
    checkmate::check_class(label_help, "html")
  )
  checkmate::assert_flag(fixed)
  checkmate::assert_flag(fixed_on_single)

  if (!is.null(width)) {
    validateCssUnit(width)
  }

  default_options <- list(
    "actions-box" = multiple,
    "none-selected-text" = "- Nothing selected -",
    "max-options" = ifelse(multiple, Inf, 1),
    "show-subtext" = TRUE,
    "live-search" = ifelse(length(choices) > 10, TRUE, FALSE)
  )

  # if called outside the fluidPage then will assume bs 3
  bs_version <- get_bs_version()
  if (isTRUE(bs_version != "3")) default_options[["style"]] <- "btn-outline-secondary"

  options <- if (!identical(options, list())) {
    c(options, default_options[setdiff(names(default_options), names(options))])
  } else {
    default_options
  }

  if (is.null(choices)) {
    choices <- ""
    selected <- NULL
  }

  if (length(choices) <= 1 && fixed_on_single) fixed <- TRUE

  raw_choices <- extract_raw_choices(choices, attr(choices, "sep"))
  raw_selected <- extract_raw_choices(selected, attr(choices, "sep"))

  ui_picker <- tags$div(
    id = paste0(inputId, "_input"),
    # visibility feature marked with display: none/block instead of shinyjs::hide/show
    #  as mechanism to hide/show is handled by javascript code
    style = if (fixed) "display: none;" else "display: block;",
    shinyWidgets::pickerInput(
      inputId = inputId,
      label = label,
      choices = raw_choices,
      selected = raw_selected,
      multiple = TRUE,
      width = width,
      options = options,
      choicesOpt = picker_options(choices)
    )
  )

  if (!is.null(label_help)) {
    ui_picker[[3]] <- append(ui_picker[[3]], list(div(class = "label-help", label_help)), after = 1)
  }

  ui_fixed <- tags$div(
    id = paste0(inputId, "_fixed"),
    # visibility feature marked with display: none/block instead of shinyjs::hide/show
    #  as mechanism to hide/show is handled by javascript code
    style = if (fixed) "display: block;" else "display: none;",
    tags$label(class = "control-label", label),
    # selected values as verbatim text
    tags$code(
      id = paste0(inputId, "_selected_text"),
      if (length(selected) > 0) {
        toString(selected)
      } else {
        "NULL"
      }
    ),
    label_help
  )

  div(
    include_css_files(pattern = "picker_input"),

    # when selected values in ui_picker change
    # then update ui_fixed - specifically, update '{id}_selected_text' element
    tags$script(
      sprintf(
        "
        $(function() {
          $('#%1$s').on('change', function(e) {
            var select_concat = $(this).val().length ? $(this).val().join(', ') : 'NULL';
            $('#%1$s_selected_text').html(select_concat);
          })
        })",
        inputId
      )
    ),

    # if ui_picker has only one or less option or is fixed then hide {id}_input and show {id}_fixed
    if (fixed_on_single) {
      js <- sprintf(
        "$(function() {
          $('#%1$s').on('change', function(e) {
            var options = $('#%1$s').find('option');
            if (options.length == 1) {
              $('#%1$s_input').hide();
              $('#%1$s_fixed').show();
            } else {
              $('#%1$s_input').show();
              $('#%1$s_fixed').hide();
            }
          })
        })",
        inputId
      )
      tags$script(js)
    },
    div(ui_picker, ui_fixed)
  )
}

#' @rdname optionalSelectInput
#' @param session (`shiny.session`)\cr
#' @export
updateOptionalSelectInput <- function(session, # nolint
                                      inputId, # nolint
                                      label = NULL,
                                      selected = NULL,
                                      choices = NULL) {
  raw_choices <- extract_raw_choices(choices, attr(choices, "sep"))
  raw_selected <- extract_raw_choices(selected, attr(choices, "sep"))

  # update picker input
  shinyWidgets::updatePickerInput(
    session = session,
    inputId = inputId,
    label = label,
    selected = as.character(raw_selected),
    choices = raw_choices,
    choicesOpt = picker_options(choices)
  )

  invisible(NULL)
}

#' Get icons to represent variable types in dataset
#'
#' @param var_type (`character`)\cr
#'  of R internal types (classes).
#'
#' @return (`character`)\cr
#'  vector of HTML icons corresponding to  data type in each column.
#' @keywords internal
#'
variable_type_icons <- function(var_type) {
  checkmate::assert_character(var_type, any.missing = FALSE)

  class_to_icon <- list(
    numeric = "arrow-up-1-9",
    integer = "arrow-up-1-9",
    logical = "pause",
    Date = "calendar",
    POSIXct = "calendar",
    POSIXlt = "calendar",
    factor = "chart-bar",
    character = "keyboard",
    primary_key = "key",
    unknown = "circle-question"
  )
  class_to_icon <- lapply(class_to_icon, function(icon_name) toString(icon(icon_name, lib = "font-awesome")))

  res <- unname(vapply(
    var_type,
    FUN.VALUE = character(1),
    FUN = function(class) {
      if (class == "") {
        class
      } else if (is.null(class_to_icon[[class]])) {
        class_to_icon[["unknown"]]
      } else {
        class_to_icon[[class]]
      }
    }
  ))

  return(res)
}

#' Optional content for `optionalSelectInput`
#'
#' Prepares content to be displayed in `optionalSelectInput` with icons and labels
#'
#' @param var_name (`character`)\cr
#'  variable name
#' @param var_label (`character`)\cr
#'  variable alternative name - for example variable label
#' @param var_type (`character`)
#' class of the variable.
#'
#' @return (`character`) HTML contents with all elements combined
#' @keywords internal
#'
picker_options_content <- function(var_name, var_label, var_type) {
  if (length(var_name) == 0) {
    return(character(0))
  }
  if (length(var_type) == 0 && length(var_label) == 0) {
    return(var_name)
  }
  checkmate::assert_character(var_name, min.len = 1, any.missing = FALSE)
  stopifnot(
    identical(var_type, character(0)) || length(var_type) == length(var_name),
    identical(var_label, character(0)) || length(var_label) == length(var_name)
  )

  var_icon <- variable_type_icons(var_type)

  res <- trimws(paste(
    var_icon,
    var_name,
    vapply(
      var_label,
      function(x) {
        ifelse(x == "", "", toString(tags$small(x, class = "text-muted")))
      },
      character(1)
    )
  ))

  return(res)
}

#' Create `choicesOpt` for `pickerInput`
#'
#' @param choices (`choices_labeled` or `character`)\cr
#'  choices vector
#'
#' @return (`list`)\cr
#'  to be passed as `choicesOpt` argument.
#' @keywords internal
picker_options <- function(choices) {
  if (inherits(choices, "choices_labeled")) {
    raw_choices <- extract_raw_choices(choices, sep = attr(choices, "sep"))
    return(
      list(
        content = picker_options_content(
          var_name  = raw_choices,
          var_label = extract_choices_labels(choices),
          var_type  = if (is.null(attr(choices, "types"))) character(0) else attr(choices, "types")
        )
      )
    )
  } else if (all(vapply(choices, inherits, logical(1), "choices_labeled"))) {
    choices <- unlist(unname(choices))
    return(
      list(content = picker_options_content(
        var_name  = choices,
        var_label = extract_choices_labels(choices),
        var_type  = if (is.null(attr(choices, "types"))) character(0) else attr(choices, "types")
      ))
    )
  } else {
    return(NULL)
  }
}

#' Extract raw values from choices
#'
#' @param choices (`choices_labeled`, `list` or `character`)\cr
#'  object containing choices
#' @param sep (`character(1)`)\cr
#'  A separator string to split the `choices` or `selected` inputs into the values of
#'  the different columns.
#' @return choices simplified
#' @keywords internal
extract_raw_choices <- function(choices, sep) {
  if (!is.null(sep)) {
    vapply(choices, paste, collapse = sep, character(1))
  } else if (inherits(choices, "choices_labeled")) {
    unname(unlist(choices))
  } else {
    choices
  }
}

#' if min or max are `NA` then the slider widget will be hidden
#'
#' @description `r lifecycle::badge("stable")`
#' Hidden input widgets are useful to have the `input[[inputId]]` variable
#' on available in the server function but no corresponding visual clutter from
#' input widgets that provide only a single choice.
#'
#' @inheritParams shiny::sliderInput
#' @param label_help (`shiny.tag`, optional)\cr
#'  an object of class `shiny.tag`. E.g. an object returned by [shiny::helpText()]
#' @param ... optional arguments to `sliderInput`
#'
#' @return (`shiny.tag`) HTML tag with `sliderInput` widget.
#'
#' @export
#'
#' @examples
#' optionalSliderInput("a", "b", 0, 1, 0.2)
optionalSliderInput <- function(inputId, label, min, max, value, label_help = NULL, ...) { # nolint
  checkmate::assert_number(min, na.ok = TRUE)
  checkmate::assert_number(max, na.ok = TRUE)
  checkmate::assert_numeric(value, min.len = 1, max.len = 2, any.missing = FALSE)

  is_na_min <- is.na(min)
  is_na_max <- is.na(max)

  hide <- is_na_min || is_na_max

  if (length(value) == 2) {
    value1 <- value[1]
    value2 <- value[2]
  } else {
    value1 <- value
    value2 <- value
  }

  if (is_na_min) {
    min <- value1 - 1
  }
  if (is_na_max) {
    max <- value2 + 1
  }

  if (min > value1 || max < value2) {
    stop("arguments inconsistent: min <= value <= max expected")
  }

  slider <- sliderInput(inputId, label, min, max, value, ...)

  if (!is.null(label_help)) {
    slider[[3]] <- append(slider[[3]], list(div(class = "label-help", label_help)), after = 1)
  }

  if (hide) {
    shinyjs::hidden(slider)
  } else {
    slider
  }
}

#' For `teal` modules we parameterize an \code{optionalSliderInput} with one argument
#' \code{value_min_max}
#'
#' @description `r lifecycle::badge("stable")`
#' The [optionalSliderInput()] function needs three arguments to determine
#' whether to hide the `sliderInput` widget or not. For `teal` modules we specify an
#' optional slider input with one argument here called `value_min_max`.
#'
#' @inheritParams optionalSliderInput
#'
#' @param value_min_max (`numeric(1)` or `numeric(3)`)\cr
#'  If of length 1 then the value gets set to that number and the `sliderInput` will be hidden.
#'  Otherwise, if it is of length three the three elements will map to `value`, `min` and `max` of
#'  the [optionalSliderInput()] function.
#'
#' @return (`shiny.tag`) HTML tag with range `sliderInput` widget.
#'
#' @export
#'
#' @examples
#'
#' optionalSliderInputValMinMax("a", "b", 1)
#' optionalSliderInputValMinMax("a", "b", c(3, 1, 5))
optionalSliderInputValMinMax <- function(inputId, label, value_min_max, label_help = NULL, ...) { # nolint
  checkmate::assert(
    checkmate::check_numeric(
      value_min_max,
      finite = TRUE,
      len = 3
    ),
    checkmate::check_numeric(
      value_min_max,
      finite = TRUE,
      len = 1
    )
  )

  x <- value_min_max

  vals <- if (length(x) == 3) {
    checkmate::assert_number(x[1], lower = x[2], upper = x[3], .var.name = "value_min_max")
    list(value = x[1], min = x[2], max = x[3])
  } else if (length(x) == 1) {
    list(value = x, min = NA_real_, max = NA_real_)
  }

  slider <- optionalSliderInput(inputId, label, vals$min, vals$max, vals$value, ...)

  if (!is.null(label_help)) {
    slider[[3]] <- append(slider[[3]], list(div(class = "label-help", label_help)), after = 1)
  }
  return(slider)
}

#' Extract labels from choices basing on attributes and names
#'
#' @param choices (`list` or `vector`)\cr
#'  select choices
#' @param values (optional)\cr
#'  choices subset for which labels should be extracted, `NULL` for all choices.
#'
#' @return (`character`) vector with labels
#' @keywords internal
extract_choices_labels <- function(choices, values = NULL) {
  res <- if (inherits(choices, "choices_labeled")) {
    attr(choices, "raw_labels")
  } else if (!is.null(names(choices)) && !setequal(names(choices), unlist(unname(choices)))) {
    names(choices)
  } else {
    NULL
  }

  if (!is.null(values) && !is.null(res)) {
    stopifnot(all(values %in% choices))
    res <- res[vapply(values, function(val) which(val == choices), numeric(1))]
  }

  return(res)
}

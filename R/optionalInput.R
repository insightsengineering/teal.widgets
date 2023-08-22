#' Hide, Show Label only or display a `pickerInput`
#'
#' @description `r lifecycle::badge("stable")`
#' Hidden input widgets are useful to have the `input[[inputId]]` variable
#' on available in the server function but no corresponding visual clutter from
#' input widgets that provide only a single choice.
#'
#' @inheritParams shinyWidgets::pickerInput
#' @param choices (`character`, `NULL`)\cr
#'   If `choices` is `NULL` no `pickerInput` widget is displayed and `input[[inputId]]`
#'   will be `""`. If `choices` is of length 1 then a label and character string will be
#'   displayed and the `pickerInput` widget will be hidden. If the length of `choices`
#'   is more than one the `pickerInput` element will be displayed.
#'   If elements of the list are named then that name rather than the value
#'   is displayed to the user.
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
#' @param width (`character(1)`)\cr
#'  The width of the input passed to `pickerInput`  e.g. `'auto'`, `'fit'`, `'100px'` or `'75%'`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' optionalSelectInput(inputId = "xvar", label = "x variable", choices = "A", selected = "A")
#' optionalSelectInput(
#'   inputId = "xvar",
#'   label = "x variable",
#'   choices = LETTERS[1:5],
#'   selected = "A"
#' )
#' optionalSelectInput(
#'   inputId = "xvar",
#'   label = "x variable",
#'   choices = c("A - value A" = "A"),
#'   selected = "A"
#' )
#'
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
#' optionalSelectInput(
#'   inputId = "xvar",
#'   label = "x variable",
#'   choices = teal.transform::variable_choices(data = data, subset = c("AGE", "SEX", "PARAMCD")),
#'   selected = "PARAMCD"
#' )
#'
#' selected_value <- paste0(lapply(data[1, c("PARAMCD", "AVISIT")], as.character), collapse = " - ")
#' optionalSelectInput(
#'   inputId = "xvar",
#'   label = "x variable",
#'   choices = teal.transform::value_choices(
#'     data = data,
#'     var_choices = c("PARAMCD", "AVISIT"),
#'     var_label = c("PARAM", "AVISIT")
#'   ),
#'   selected = selected_value
#' )
#' }
optionalSelectInput <- function(inputId, # nolint
                                label = NULL,
                                choices = NULL,
                                selected = NULL,
                                multiple = FALSE,
                                sep = NULL,
                                options = list(),
                                label_help = NULL,
                                fixed = FALSE,
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
  stopifnot(is.list(options))
  checkmate::assert(
    checkmate::check_string(label_help, null.ok = TRUE),
    checkmate::check_class(label_help, "shiny.tag"),
    checkmate::check_class(label_help, "shiny.tag.list"),
    checkmate::check_class(label_help, "html")
  )
  checkmate::assert_flag(fixed)

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

  raw_choices <- extract_raw_choices(choices, attr(choices, "sep"))
  raw_selected <- extract_raw_choices(selected, attr(choices, "sep"))

  ui <- shinyWidgets::pickerInput(
    inputId = inputId,
    label = label,
    choices = raw_choices,
    selected = raw_selected,
    multiple = TRUE,
    width = width,
    options = options,
    choicesOpt = picker_options(choices)
  )

  if (!is.null(label_help)) {
    ui[[3]] <- append(ui[[3]], list(div(class = "label-help", label_help)), after = 1)
  }

  shiny::tagList(
    include_css_files(pattern = "picker_input"),
    if (is.null(choices)) {
      shinyjs::hidden(ui)
    } else {
      if (fixed) {
        div(
          shinyjs::hidden(ui),
          tags$label(id = paste0(inputId, "_textonly"), class = "control-label", sub(":[[:space:]]+$", "", label)),
          if (length(selected) > 0) {
            tags$code(
              id = paste0(inputId, "_valueonly"),
              paste(selected, collapse = ", ")
            )
          },
          label_help
        )
      } else {
        ui
      }
    }
  )
}

#' Update `optionalSelectInput`
#'
#' @description `r lifecycle::badge("stable")`
#'
#' @inheritParams shinyWidgets::updatePickerInput
#'
#' @return `NULL`
#'
#' @export
#'
updateOptionalSelectInput <- function(session, # nolint
                                      inputId, # nolint
                                      label = NULL,
                                      selected = NULL,
                                      choices = NULL) {
  raw_choices <- extract_raw_choices(choices, attr(choices, "sep"))
  raw_selected <- extract_raw_choices(selected, attr(choices, "sep"))

  shinyWidgets::updatePickerInput(
    session = session,
    inputId = inputId,
    label = label,
    selected = as.character(raw_selected),
    choices = raw_choices,
    choicesOpt = picker_options(choices)
  )

  shinyjs::show(inputId)
  shinyjs::hide(paste0(inputId, "_textonly"))

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
#' @examples
#' teal.widgets:::variable_type_icons(c(
#'   "integer", "numeric", "logical", "Date", "POSIXct", "POSIXlt",
#'   "factor", "character", "unknown", ""
#' ))
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
#' @examples
#' teal.widgets:::picker_options_content(
#'   var_name = c("SEX", "BRMRKR1"),
#'   var_label = c("Sex", "Biomarker 1"),
#'   var_type = c("factor", "numeric")
#' )
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
#' @export
#'
#' @examples
#'
#' optionalSliderInputValMinMax("a", "b", 1)
#' optionalSliderInputValMinMax("a", "b", c(3, 1, 5))
optionalSliderInputValMinMax <- function(inputId, label, value_min_max, label_help = NULL, ...) { # nolint

  x <- value_min_max

  checkmate::assert_numeric(x, .var.name = "value_min_max")

  vals <- if (length(x) == 3) {
    if (any(diff(x[c(2, 1, 3)]) < 0)) {
      stop(paste("value_min_max is expected to be (value, min, max) where min <= value <= max"))
    }
    list(value = x[1], min = x[2], max = x[3])
  } else if (length(x) == 1) {
    list(value = x, min = NA_real_, max = NA_real_)
  } else {
    stop(paste("value_min_max is expected to be of length 1 (value) or of length 3 (value, min, max)"))
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

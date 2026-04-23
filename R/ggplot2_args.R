#' Creates `ggplot2_args` object
#'
#' @description
#' Constructor of `ggplot2_args` class of objects.
#' The `ggplot2_args` argument should be a part of every module which contains any `ggplot2` graphics.
#' The function arguments are validated to match their `ggplot2` equivalents.
#'
#' For more details see the vignette: `vignette("custom-ggplot2-arguments", package = "teal.widgets")`.
#'
#' @seealso
#' * [resolve_ggplot2_args()] to resolve multiple objects into one using pre-defined priorities.
#' * [parse_ggplot2_args()] to parse resolved list into list of calls.
#'
#' @param labs (named `list`)\cr
#'  where all fields have to match [ggplot2::labs()] arguments.
#' @param theme (named `list`)\cr
#'  where all fields have to match [ggplot2::theme()] arguments.
#'
#' @return (`ggplot2_args`) object.
#' @export
#' @examples
#' ggplot2_args(
#'   labs = list(title = "TITLE"),
#'   theme = list(title = ggplot2::element_text(size = 20))
#' )
ggplot2_args <- function(labs = list(), theme = list()) {
  checkmate::assert_list(labs)
  checkmate::assert_list(theme)
  checkmate::assert_character(names(labs), unique = TRUE, null.ok = TRUE)
  checkmate::assert_character(names(theme), unique = TRUE, null.ok = TRUE)

  ggplot2_theme <- methods::formalArgs(ggplot2::theme)
  # utils::getFromNamespace is not recommended nevertheless needed as it is replacing `:::`.
  # usage of static values will be vulnerable to any changes in ggplot2 aesthetics.
  ggplot2_labs <- c(
    utils::getFromNamespace(".all_aesthetics", "ggplot2"),
    methods::formalArgs(ggplot2::labs)
  )
  checkmate::assert_subset(names(labs), choices = ggplot2_labs, empty.ok = TRUE)
  checkmate::assert_subset(names(theme), choices = ggplot2_theme, empty.ok = TRUE)

  structure(list(labs = labs, theme = theme), class = "ggplot2_args")
}

#' Resolving and reducing multiple `ggplot2_args` objects
#'
#' @description
#' Resolving and reducing multiple `ggplot2_args` objects.
#' This function is intended to utilize user provided settings, defaults provided by the module creator and
#' also `teal` option. See `Details`, below, to understand the logic.
#'
#' @seealso [parse_ggplot2_args()] to parse resolved list into list of calls.
#'
#' @param user_plot (`ggplot2_args`)\cr
#'  end user setup for theme and labs in the specific plot.
#'  Created with the [ggplot2_args()] function. The `NULL` value is supported.
#' @param user_default (`ggplot2_args`)\cr
#'  end user setup for module default theme and labs.
#'  Created with the [ggplot2_args()] function. The `NULL` value is supported.
#' @param module_plot (`ggplot2_args`)\cr
#'  module creator setup for theme and labs in the specific plot.
#'  Created with the [ggplot2_args()] function. The `NULL` value is supported.
#' @param app_default (`ggplot2_args`)\cr
#'  Application level setting. Can be `NULL`.
#'
#' @return `ggplot2_args` object.
#'
#' @details
#' The function picks the first non `NULL` value for each argument, checking in the following order:
#' 1. `ggplot2_args` argument provided by the end user.
#' Per plot (`user_plot`) and then default (`user_default`) setup.
#' 2. `app_default` global R variable, `teal.ggplot2_args`.
#' 3. `module_plot` which is a module creator setup.
#' @export
#' @examples
#' resolve_ggplot2_args(
#'   user_plot = ggplot2_args(
#'     labs = list(title = "TITLE"),
#'     theme = list(title = ggplot2::element_text(size = 20))
#'   ),
#'   user_default = ggplot2_args(
#'     labs = list(x = "XLAB")
#'   )
#' )
resolve_ggplot2_args <- function(user_plot = ggplot2_args(),
                                 user_default = ggplot2_args(),
                                 module_plot = ggplot2_args(),
                                 app_default = getOption("teal.ggplot2_args", ggplot2_args())) {
  checkmate::assert_class(user_plot, "ggplot2_args", null.ok = TRUE)
  checkmate::assert_class(user_default, "ggplot2_args", null.ok = TRUE)
  checkmate::assert_class(module_plot, "ggplot2_args", null.ok = TRUE)
  checkmate::assert_class(app_default, "ggplot2_args", null.ok = TRUE)

  ggplot2_args_all <- list(
    "plot" = user_plot,
    "default" = user_default,
    "teal" = app_default,
    "module" = module_plot
  )

  labs_args <- Reduce(`c`, lapply(ggplot2_args_all, function(x) x$labs))
  labs_args <- if (is.null(labs_args)) list() else labs_args[!duplicated(names(labs_args))]

  theme_args <- Reduce(`c`, lapply(ggplot2_args_all, function(x) x$theme))
  theme_args <- if (is.null(theme_args)) list() else theme_args[!duplicated(names(theme_args))]

  ggplot2_args(labs = labs_args, theme = theme_args)
}

#' Parse `ggplot2_args` object into the `ggplot2` expression
#'
#' @description
#' A function to parse expression from the `ggplot2_args` object.
#' @param ggplot2_args (`ggplot2_args`)\cr
#'  This argument could be a result of the [resolve_ggplot2_args()].
#' @param ggtheme (`character(1)`)\cr
#'  name of the `ggplot2` theme to be applied, e.g. `"dark"`.
#'
#' @return (`list`) of up to three elements of class `languange`: `"labs"`, `"ggtheme"` and `"theme"`.
#' @export
#' @examples
#' parse_ggplot2_args(
#'   resolve_ggplot2_args(ggplot2_args(
#'     labs = list(title = "TITLE"),
#'     theme = list(title = ggplot2::element_text(size = 20))
#'   ))
#' )
#'
#' parse_ggplot2_args(
#'   resolve_ggplot2_args(
#'     ggplot2_args(
#'       labs = list(title = "TITLE"),
#'       theme = list(title = ggplot2::element_text(size = 20))
#'     )
#'   ),
#'   ggtheme = "gray"
#' )
parse_ggplot2_args <- function(ggplot2_args = teal.widgets::ggplot2_args(),
                               ggtheme = c(
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
                               )) {
  checkmate::assert_class(ggplot2_args, "ggplot2_args")
  ggtheme <- match.arg(ggtheme)

  res_list <- list()

  labs_args <- ggplot2_args$labs

  labs_f <- if (length(labs_args)) {
    as.call(c(list(quote(ggplot2::labs)), labs_args))
  } else {
    NULL
  }

  default_theme <- if (ggtheme != "default") {
    as.call(list(str2lang(paste0("ggplot2::theme_", ggtheme))))
  } else {
    NULL
  }

  theme_args <- lapply(ggplot2_args$theme, to_call)

  theme_f <- if (length(theme_args)) {
    as.call(c(list(quote(ggplot2::theme)), theme_args))
  } else {
    NULL
  }

  final_list <- Filter(Negate(is.null), list(labs = labs_f, ggtheme = default_theme, theme = theme_f))
  # For empty final_list we want to return empty list, not empty named list
  `if`(length(final_list) == 0, list(), final_list)
}

#' Convert an evaluated ggplot2 theme element to its call representation
#'
#' Ensures that evaluated element objects (e.g. from `element_text(size = 20)`)
#' produce clean, deparseable calls instead of opaque `structure(...)` output
#' that may fail to reconstruct correctly after a deparse/parse round-trip.
#'
#' Already-unevaluated calls and plain atomic values pass through unchanged.
#'
#' @param x An R object, typically a value from the `theme` list of a
#'   [ggplot2_args()] object.
#' @return A call or atomic value suitable for embedding in an `as.call()`
#'   expression.
#' @keywords internal
to_call <- function(x) {
  if (is.null(x) || is.call(x) || is.name(x)) return(x)
  if (inherits(x, "element_blank")) return(quote(ggplot2::element_blank()))
  if (inherits(x, "waiver")) return(quote(ggplot2::waiver()))
  if (inherits(x, "rel")) {
    return(as.call(list(str2lang("ggplot2::rel"), as.numeric(x))))
  }
  # Check both bare and namespaced class (ggplot2 < 4.0 vs >= 4.0)
  if (inherits(x, "margin") || inherits(x, "ggplot2::margin")) return(margin_to_call(x))
  if (inherits(x, "unit")) return(unit_to_call(x))
  if (inherits(x, "element")) return(element_to_call(x))
  if (is.atomic(x)) return(x)
  warning(
    "Theme argument of class ", paste(class(x), collapse = "/"),
    " could not be converted to a call. ",
    "Wrap the value in quote() to ensure a clean deparse/parse round-trip.",
    call. = FALSE
  )
  x
}

#' @keywords internal
element_to_call <- function(x) {
  cls <- sub("^.*::", "", class(x)[1])
  fn <- str2lang(paste0("ggplot2::", cls))
  constructor <- utils::getFromNamespace(cls, "ggplot2")
  default_obj <- tryCatch(constructor(), error = function(e) NULL)
  # "color" is an alias for "colour" in ggplot2
  param_names <- setdiff(names(formals(constructor)), c("...", "inherit.blank", "color"))

  args <- list()
  for (nm in param_names) {
    val <- tryCatch(x[[nm]], error = function(e) NULL)
    if (is.null(val)) next
    if (!is.null(default_obj)) {
      default_val <- tryCatch(default_obj[[nm]], error = function(e) NULL)
      if (identical(val, default_val)) next
    }
    args[[nm]] <- to_call(val)
  }
  as.call(c(list(fn), args))
}

#' @keywords internal
margin_to_call <- function(x) {
  vals <- as.numeric(x)
  u <- grid::unitType(x)[1]
  default_margin <- ggplot2::margin()
  if (identical(vals, as.numeric(default_margin)) && identical(u, grid::unitType(default_margin)[1])) {
    return(quote(ggplot2::margin()))
  }
  as.call(list(str2lang("ggplot2::margin"), vals[1], vals[2], vals[3], vals[4], u))
}

#' @keywords internal
unit_to_call <- function(x) {
  vals <- as.numeric(x)
  u <- grid::unitType(x)
  if (length(vals) == 1L) {
    as.call(list(str2lang("grid::unit"), vals, u[1]))
  } else {
    as.call(list(str2lang("grid::unit"), vals, u))
  }
}

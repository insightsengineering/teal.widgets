#' Builds a `basic_table_args` object
#'
#' @description
#' This function has to be used to build an input for a `basic_table_args` argument.
#' The `basic_table_args` argument should be a part of every module which contains any `rtables` object.
#' Arguments are validated to match their `rtables` equivalents.
#'
#' For more details see the vignette: `vignette("custom-basic-table-arguments", package = "teal.widgets")`.
#'
#' @seealso
#' * [resolve_basic_table_args()] to resolve multiple objects into one using pre-defined priorities.
#' * [parse_basic_table_args()] to parse resolved list into list of calls.
#'
#' @param ... arguments compatible with [rtables::basic_table()].
#'
#' @return (`basic_table_args`) object.
#' @export
#' @examples
#' basic_table_args(subtitles = "SUBTITLE")
basic_table_args <- function(...) {
  table_args_l <- list(...)
  checkmate::assert_character(names(table_args_l), unique = TRUE, null.ok = TRUE)

  basic_table_formals <- methods::formalArgs(rtables::basic_table)
  checkmate::assert_subset(names(table_args_l), choices = basic_table_formals, empty.ok = TRUE)

  structure(table_args_l, class = "basic_table_args")
}

#' Resolves and reduces multiple `basic_table_args` objects
#'
#' @description
#' Resolving and reducing multiple `basic_table_args` objects.
#' This function is intended to utilize user provided settings, defaults provided by the module creator and
#' also `teal` option. See `Details`, below, to understand the logic.
#'
#' @seealso [parse_basic_table_args()] to parse resolved list into list of calls.
#'
#' @param user_table (`basic_table_args`)\cr
#'  end user setup for [rtables::basic_table()] of a specific table.
#'  Created with the [basic_table_args()] function. The `NULL` value is supported.
#' @param user_default (`basic_table_args`)\cr
#'  end user default setup for [rtables::basic_table()]
#'  of a specific table. Created with the [basic_table_args()] function. The `NULL` value is supported.
#' @param module_table (`ggplot2_args`)\cr
#'  module creator setup for [rtables::basic_table()] of a specific table.
#'  Created with the [basic_table_args()] function. The `NULL` value is supported.
#' @param app_default (`basic_table_args`)\cr
#'  Application level setting. Can be `NULL`.
#'
#' @return `basic_table_args` object.
#' @details
#' The function picks the first non `NULL` value for each argument, checking in the following order:
#' 1. `basic_table_args` argument provided by the end user.
#' Per table (`user_table`) and then default (`user_default`) setup.
#' 2. `app_default` global R variable, `teal.basic_table_args`.
#' 3. `module_table` which is a module creator setup.
#' @export
#' @examples
#' resolve_basic_table_args(
#'   user_table = basic_table_args(title = "TITLE"),
#'   user_default = basic_table_args(title = "DEFAULT_TITLE", subtitles = "SUBTITLE")
#' )
resolve_basic_table_args <- function(user_table = basic_table_args(),
                                     user_default = basic_table_args(),
                                     module_table = basic_table_args(),
                                     app_default = getOption("teal.basic_table_args", basic_table_args())) {
  checkmate::assert_class(user_table, "basic_table_args", null.ok = TRUE)
  checkmate::assert_class(user_default, "basic_table_args", null.ok = TRUE)
  checkmate::assert_class(module_table, "basic_table_args", null.ok = TRUE)
  checkmate::assert_class(app_default, "basic_table_args", null.ok = TRUE)

  table_args_all <- list(
    "table" = user_table,
    "default" = user_default,
    "teal" = app_default,
    "module" = module_table
  )

  table_args_f <- Reduce(`c`, table_args_all)

  if (length(table_args_f) == 0) {
    basic_table_args()
  } else {
    do.call(basic_table_args, table_args_f[!duplicated(names(table_args_f))])
  }
}

#' Parses `basic_table_args` object into the `basic_table` expression
#'
#' @description
#' A function to parse expression from the `basic_table_args` object.
#' @param basic_table_args (`basic_table_args`)\cr
#'  This argument could be a result of the [`resolve_basic_table_args()`].
#'
#' @return (`language`) the `rtables::basic_table()` filled with additional arguments.
#' @export
#' @examples
#' parse_basic_table_args(
#'   resolve_basic_table_args(
#'     user_table = basic_table_args(title = "TITLE"),
#'     user_default = basic_table_args(title = "DEFAULT_TITLE", subtitles = "SUBTITLE")
#'   )
#' )
parse_basic_table_args <- function(basic_table_args = teal.widgets::basic_table_args()) {
  checkmate::assert_class(basic_table_args, "basic_table_args", null.ok = TRUE)

  if (length(basic_table_args) == 0) {
    quote(rtables::basic_table())
  } else {
    as.call(c(list(quote(rtables::basic_table)), basic_table_args))
  }
}

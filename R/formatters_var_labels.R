#' Get Label Attributes of Variables in a \code{data.frame}
#'
#' Variable labels can be stored as a \code{label} attribute for each variable.
#' This functions returns a named character vector with the variable labels
#' (empty sting if not specified)
#'
#' @param x a \code{data.frame} object
#' @param fill boolean in case the \code{label} attribute does not exist if
#'   \code{TRUE} the variable names is returned, otherwise \code{NA}
#'
#' @return a named character vector with the variable labels, the names
#'   correspond to the variable names
#'
#' @export
#'
#' @examples
#' x <- iris
#' formatters_var_labels(x)
formatters_var_labels <- function(x, fill = FALSE) {
  stopifnot(is.data.frame(x))
  if (NCOL(x) == 0) {
    return(character())
  }

  y <- Map(function(col, colname) {
    label <- attr(col, "label")

    if (is.null(label)) {
      if (fill) {
        colname
      } else {
        NA_character_
      }
    } else {
      if (!is.character(label) && !(length(label) == 1)) {
        stop("label for variable ", colname, "is not a character string")
      }
      as.vector(label)
    }
  }, x, colnames(x))

  labels <- unlist(y, recursive = FALSE, use.names = TRUE)

  if (!is.character(labels)) {
    stop("label extraction failed")
  }

  labels
}

#' Copy and Change Variable Labels of a \code{data.frame}
#'
#' Relabel a subset of the variables
#'
#' @inheritParams var_labels<-
#' @param ... name-value pairs, where name corresponds to a variable name in
#'   \code{x} and the value to the new variable label
#'
#' @return a copy of \code{x} with changed labels according to \code{...}
#'
#' @export
#'
#' @examples
#' x <- formatters_var_relabel(iris, Sepal.Length = "Sepal Length of iris flower")
#' formatters_var_labels(x)
#'
formatters_var_relabel <- function(x, ...) {
  # todo: make this function more readable / code easier
  stopifnot(is.data.frame(x))
  if (missing(...)) {
    return(x)
  }
  dots <- list(...)
  varnames <- names(dots)
  stopifnot(!is.null(varnames))

  map_varnames <- match(varnames, colnames(x))

  if (any(is.na(map_varnames))) {
    stop("variables: ", paste(varnames[is.na(map_varnames)], collapse = ", "), " not found")
  }

  if (any(vapply(dots, Negate(is.character), logical(1)))) {
    stop("all variable labels must be of type character")
  }

  for (i in seq_along(map_varnames)) {
    attr(x[[map_varnames[[i]]]], "label") <- dots[[i]]
  }

  x
}


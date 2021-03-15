#' Semantics access
#'
#' Get and set the global semantics. TODO: more documentation.
#'
#' @param axes,angles semantics (see `?semantics_axes` for more details)
#'
#'   Note that missing values in the `set_dddr_semantics` call are left
#'   unchanged. Values must be explicitly `NULL` to unset semantics.
#'
#' @name semantics_access
NULL

#' @rdname semantics_access
#' @export
set_dddr_semantics <- function(axes, angles) {
  if (!missing(axes)) {
    options("dddr.semantics.axes" = axes)
  }
  if (!missing(angles)) {
    options("dddr.semantics.angles" = angles)
  }
}

#' @rdname semantics_access
#' @export
get_angles_semantics <- function() {
  getOption("dddr.semantics.angles")
}

#' @rdname semantics_access
#' @export
get_axes_semantics <- function() {
  getOption("dddr.semantics.axes")
}

#' @rdname semantics_access
#' @export
get_dddr_semantics <- function() {
  list(
    axes = get_axes_semantics(),
    angles = get_angles_semantics()
  )
}

#' Predefined axes semantics
#'
#' A collection of predefined axis and angle semantics are provided here.
#'
#' The correctness is, of course, dependent upon whether the semantics are
#' followed in the virtual world. These are no substitute for visual
#' inspection.
#'
#' @name common_semantics
NULL

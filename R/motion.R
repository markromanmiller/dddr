#' Linear or constant approximation of vector3
#'
#' Using `stats::approx`, create a linear or constant approximation
#' (interpolation) of a value or vector
#'
#' @param x Indexes for the input values
#' @param y The values or vectors to be interpolated between
#' @param xout Indexes for the output values
#' @param ... Additional arguments passed on to stats::approx
#'
#' @export
approx <- function(x, y, xout, ...) {
  UseMethod("approx", y)
}

#' @method approx dddr_vector3
#' @export
approx.dddr_vector3 <- function(x, y, xout, ...) {
  # I have reservations abotu this method. On one hand, it's nice that it
  # mirrors base R, on the other hand, base R isn't worth mirroring here.

  list(
    x = x,
    y = vector3(
      x = stats::approx(x, y$x, xout, ...)$y,
      y = stats::approx(x, y$y, xout, ...)$y,
      z = stats::approx(x, y$z, xout, ...)$y
    )
  )
}

#' @method approx default
#' @export
approx.default <- function(x, y, xout, ...) {
  stats::approx(x, y, xout, ...)
}

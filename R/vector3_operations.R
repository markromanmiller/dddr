
#' @describeIn vector3_arith
#'
#' @importFrom vctrs vec_arith
#' @method vec_arith vrm_vector3
#' @export vec_arith.vrm_vector3
#' @export
vec_arith.vrm_vector3 <- function(op, x, y, ...) {
  UseMethod("vec_arith.vrm_vector3", y)
}

#' @method vec_arith.vrm_vector3 default
#' @export
vec_arith.vrm_vector3.default <- function(op, x, y, ...) {
  vctrs::stop_incompatible_op(op, x, y)
}

#' @method vec_arith.vrm_vector3 vrm_vector3
#' @export
vec_arith.vrm_vector3.vrm_vector3 <- function(op, x, y, ...) {
  switch(
    op,
    "+" = ,
    "-" =
      new_vector3(
        x = vctrs::vec_arith_base(op, x$x, y$x),
        y = vctrs::vec_arith_base(op, x$y, y$y),
        z = vctrs::vec_arith_base(op, x$z, y$z)
      ),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @method vec_arith.vrm_vector3 numeric
#' @export
vec_arith.vrm_vector3.numeric <- function(op, x, y, ...) {
  switch(
    op,
    "+" = ,
    "-" = {
      if (length(y) != 3) {
        vctrs::stop_incompatible_op(op, x, y, details="the numeric adding to a vector3 must have length 3")
      }
      new_vector3(
        x = vctrs::vec_arith_base(op, x$x, y[1]),
        y = vctrs::vec_arith_base(op, x$y, y[2]),
        z = vctrs::vec_arith_base(op, x$z, y[3])
      )},
    vctrs::stop_incompatible_op(op, x, y),
    "*" = ,
    "/" = {
      if (length(y) != 1 && length(y) != length(x)) {
        vctrs::stop_incompatible_op(op, x, y, details="the numeric multiply a vector3 must have length 1 or same length as the vector")
      }
      new_vector3(
        x = vctrs::vec_arith_base(op, x$x, y),
        y = vctrs::vec_arith_base(op, x$y, y),
        z = vctrs::vec_arith_base(op, x$z, y)
      )}
  )
}

#' @method vec_arith.vrm_vector3 MISSING
#' @export
vec_arith.vrm_vector3.MISSING <- function(op, x, y, ...) {
  switch(
    op,
    "-" = new_vector3(x=-x$x, y=-x$y, z=-x$z),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @importFrom vctrs vec_arith.numeric
#' @method vec_arith.numeric vrm_vector3
#' @export
vec_arith.numeric.vrm_vector3 <- function(op, x, y, ...) {
  switch(
    op,
    "+" = ,
    "*" = vec_arith.vrm_vector3.numeric(op, y, x, ...),
    #"-" = vec_arith.vrm_vector3.numeric("+", y, -x, ...),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @importFrom vctrs vec_math
#' @method vec_math vrm_vector3
#' @export
vec_math.vrm_vector3 <- function(.fn, .x, ...) {
  switch(
    .fn,
    "sum" = new_vector3(
      x = sum(.x$x),
      y = sum(.x$y),
      z = sum(.x$z)
    ),
    "cumsum" = new_vector3(
      x = cumsum(.x$x),
      y = cumsum(.x$y),
      z = cumsum(.x$z)
    ),
    "mean" = new_vector3(
      x = mean(.x$x),
      y = mean(.x$y),
      z = mean(.x$z)
    ),
    stop("Incompatible summary generic")
  )
}

#' Determine the distance of a vector or between two vectors
#'
#' @param to
#' @param from Optional. Instead of length being calculated from the origin, it is
#' calculated from this point instead.
#'
#' @return A numeric vector of distances
#'
#' @export
distance <- function(to, from=NULL) {
  if (!is.null(from)) {
    to <- to - from
  }
  sqrt(to$x^2 + to$y^2 + to$z^2)
}

#' Normalize vector length
#'
#' `normalize` scales vectors so that direction is preserved but length can be varied.
#' The primary usage is to convert a vector to unit length, i.e, a length of 1. This function accepts a second argument, `length`,
#' to set a certain value for the magnitude of the vector.
#'
#' @param v The vectors to normalize
#' @param length (Optional) Normalized vectors will have this length. Both length-1 and length-N numerics are accepted here.
#'
#' @export
normalize <- function(v, length=1) {
  length * v / distance(v)
}

#' @export
cross <- function(pointer, middle, handedness="settings") {
  # use settings default, or left or right.
  # TODO: how to get "handedness" to show up as settings, right, left in the documentation
  stop("Not Implemented Yet")
}

#' @export
dot <- function(a, b) {
  stop("Not Implemented Yet")
}





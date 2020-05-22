#' Arithmetic operations on quaternions
#'
#' Quaternions can be multiplied and conjugated. This can be done with vectors
#' of type `quat` or with numeric vectors of reasonable lengths.
#'
#' For multiplication, if a numeric vector is used, it can only be length 4. The
#' entries of this vector are interpreted as `w,x,y,z` values of a quaternion. Then, the created quaternion is broadcast across all elements of the original quaternion vector.
#'
#' Note that quaternion multiplication is not communative.
#'
#' @param op Arithmetic operation, i.e, `"*"` only
#' @param x,y,z Operands
#' @param ... Unused; present for extensibility
#'
#' @seealso vector3_math
#'
#' @name quat_arith
NULL

#' @rdname quat_arith
#' @importFrom vctrs vec_arith
#' @method vec_arith dddr_quat
#' @export
vec_arith.dddr_quat <- function(op, x, y, ...) {
  UseMethod("vec_arith.dddr_quat", y)
}

#' @rdname quat_arith
#' @method vec_arith.dddr_quat default
#' @export
vec_arith.dddr_quat.default <- function(op, x, y, ...) {
  vctrs::stop_incompatible_op(op, x, y)
}

#' @rdname quat_arith
#' @method vec_arith.dddr_quat dddr_quat
#' @export
vec_arith.dddr_quat.dddr_quat <- function(op, x, y, ...) {
  switch(
    op,
    "*" = {
      a <- x # better naming
      b <- y

      new_quat(
        w = a$w * b$w - a$x * b$x - a$y * b$y - a$z * b$z,
        x = a$w * b$x + a$x * b$w + a$y * b$z - a$z * b$y,
        y = a$w * b$y - a$x * b$z + a$y * b$w + a$z * b$x,
        z = a$w * b$z + a$x * b$y - a$y * b$x + a$z * b$w
      )
    },
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @rdname quat_arith
#' @method vec_arith.dddr_quat numeric
#' @export
vec_arith.dddr_quat.numeric <- function(op, x, y, ...) {
  switch(
    op,
    "*" = {
      vec_arith.dddr_quat.dddr_quat(op, x, upgrade_to_quat(y), ...)
      },
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @rdname quat_arith
#' @importFrom vctrs vec_arith.numeric
#' @method vec_arith.numeric dddr_quat
#' @export
vec_arith.numeric.dddr_quat<- function(op, x, y, ...) {
  switch(
    op,
    "*" = {
      vec_arith.dddr_quat.dddr_quat(op, upgrade_to_quat(x), y, ...)
    },
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @rdname quat_arith
#' @export
`Conj.dddr_quat` <- function(z) {
  new_quat(
    w =  z$w,
    x = -z$x,
    y = -z$y,
    z = -z$z
  )
}

#' @export
`all.equal.dddr_quat` <- function(target, current, ...) {
  # should this be done by the quats function, i.e, where it sends two basis vectors?
  a <- target
  b <- current
  x_basis <- quat(w=0, x=1, y=0, z=0)
  y_basis <- quat(w=0, x=0, y=1, z=0)

  # TODO: convert this to an internal rotation function call
  a_x <- a * x_basis * Conj(a)
  b_x <- b * x_basis * Conj(b)
  a_y <- a * y_basis * Conj(a)
  b_y <- b * y_basis * Conj(b)

  # w's are dropped when converting back to vector
  # TODO: maybe make this more clear how different they are?
  isTRUE(all.equal(a_x$x, b_x$x, ...)) &&
    isTRUE(all.equal(a_x$y, b_x$y, ...)) &&
    isTRUE(all.equal(a_x$z, b_x$z, ...)) &&
    isTRUE(all.equal(a_y$x, b_y$x, ...)) &&
    isTRUE(all.equal(a_y$y, b_y$y, ...)) &&
    isTRUE(all.equal(a_y$z, b_y$z, ...))
}



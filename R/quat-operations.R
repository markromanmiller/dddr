#' Arithmetic operations on quaternions
#'
#' Quaternions can be multiplied and conjugated. This can be done with vectors
#' of type `quat` or with numeric vectors of reasonable lengths.
#'
#' For multiplication, if a numeric vector is used, it can only be length 4. The
#' entries of this vector are interpreted as `w,x,y,z` values of a quaternion.
#' Then, the created quaternion is broadcast across all elements of the original
#' quaternion vector.
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
#' @export
`Conj.dddr_quat` <- function(z) {
  new_quat(
    w =  z$w,
    x = -z$x,
    y = -z$y,
    z = -z$z
  )
}

#' Quaternion equality
#'
#' Like most mathematical objects, there is a notion of equality on quaternions.
#' Because the focus here is on using quaternions to rotate, quaternions are
#' considered equal if they produce the same rotation.
#'
#' In particular, if all entries in a quaternion are negated, the result is the
#' same exact rotation. This is known as "double cover." In this case, the
#' quaternions (w,x,y,z) and (-w,-x,-y,-z) are considered equal.
#'
#' The assorted proxy methods create a data frame that shows where the unit x
#' and unit y vectors are mapped in 3d space. This is more numerically stable
#' than normalizing based on e.g. sign of w.
#'
#' @param x Quaternions to form a proxy for.
#' @param ... arguments passed on the underlying function for all.equal
#' @param target,current quaternions to compare
#' @param path Path describing the proxy operation
#'
#' @name quat_equal
NULL

quat_proxy_equal <- function(x, ...) {
  q <- x
  x <- rotate(vector3(x = 1, y = 0, z = 0), rotator = q)
  y <- rotate(vector3(x = 0, y = 1, z = 0), rotator = q)

  data.frame(
    x_x = x$x, x_y = x$y, x_z = x$z,
    y_x = y$x, y_y = y$y, y_z = y$z
  )
}

#' @rdname quat_equal
#' @importFrom vctrs vec_proxy_equal
#' @method vec_proxy_equal dddr_quat
#' @export
vec_proxy_equal.dddr_quat <- function(x, ...) {
  quat_proxy_equal(x, ...)
}

#' @rdname quat_equal
#' @method all.equal dddr_quat
#' @export
`all.equal.dddr_quat` <- function(target, current, ...) {
  all.equal(quat_proxy_equal(target), quat_proxy_equal(current), ...)
}

#' @rdname quat_equal
#' @export
compare_proxy.dddr_quat <- function(x, path) {
  list(object = quat_proxy_equal(x), path = paste0("quat_proxy_equal(", path, ")"))
}

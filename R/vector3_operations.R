
#' Arithmetic operations on vectors
#'
#' Vector3s can be added, subtracted, and scaled. This can be done with vectors
#' of type `vector3` or with numeric vectors of reasonable lengths.
#'
#' For addition or subtraction, numeric vectors need to be length 3. This vector
#' is then interpreted as giving the x, y, and z elements, and then is broadcast
#' across all vector3 entries.
#'
#' For multiplication or division, numeric vectors can be length 1, which is
#' broadcast across all entries, or can be the same length as the vector3
#' vector, where each element is scaled individually.
#'
#' @param op Arithmetic operation, i.e, `"+"`, `"-"`, `"*"`, or `"/"`
#' @param x,y Operands
#' @param ... Unused; present for extensibility
#'
#' @examples
#' vector3(x = 1:4, y = 2:5, z = 3:6) + vector3(x = 2, y = 0, z = -1)
#' vector3(x = 1:4, y = 2:5, z = 3:6) + c(2, 0, -1)
#'
#' vector3(x = 1:4, y = 2:5, z = 3:6) * 4
#' vector3(x = 1:4, y = 2:5, z = 3:6) * c(3, 1, 4, 1)
#' @seealso vector3_math
#'
#' @name vector3_arith
NULL

vector3_help_message <-
  "See `?vector3_arith` for more information on vector arithmetic."

#' @rdname vector3_arith
#'
#' @importFrom vctrs vec_arith
#' @method vec_arith dddr_vector3
#' @export vec_arith.dddr_vector3
#' @export
vec_arith.dddr_vector3 <- function(op, x, y, ...) {
  UseMethod("vec_arith.dddr_vector3", y)
}

#' @rdname vector3_arith
#'
#' @method vec_arith.dddr_vector3 default
#' @export
vec_arith.dddr_vector3.default <- function(op, x, y, ...) {
  vctrs::stop_incompatible_op(op, x, y, details = vector3_help_message)
}

#' @rdname vector3_arith
#'
#' @method vec_arith.dddr_vector3 dddr_vector3
#' @export
vec_arith.dddr_vector3.dddr_vector3 <- function(op, x, y, ...) { # nolint
  switch(
    op,
    "+" = , # nolint
    "-" =
      new_vector3(
        x = vctrs::vec_arith_base(op, x$x, y$x),
        y = vctrs::vec_arith_base(op, x$y, y$y),
        z = vctrs::vec_arith_base(op, x$z, y$z)
      ),
    "*" = vctrs::stop_incompatible_op(
      op, x, y,
      details = paste(
        vector3_help_message,
        "Did you mean `dot` or `cross`?",
        "Refer to `?vector3_prod` for vector multiplication."
      )
    ),
    vctrs::stop_incompatible_op(op, x, y, details = vector3_help_message)
  )
}

#' @rdname vector3_arith
#' @method vec_arith.dddr_vector3 numeric
#' @export
vec_arith.dddr_vector3.numeric <- function(op, x, y, ...) {
  switch(
    op,
    "+" = , # nolint
    "-" = {
      if (length(y) != 3) {
        vctrs::stop_incompatible_op(
          op, x, y,
          details = paste(
            "To add or subtract a numeric and a vector3,",
            "the numeric must have length 3.",
            vector3_help_message
          )
        )
      }
      new_vector3(
        x = vctrs::vec_arith_base(op, x$x, y[1]),
        y = vctrs::vec_arith_base(op, x$y, y[2]),
        z = vctrs::vec_arith_base(op, x$z, y[3])
      )
    },
    "*" = , # nolint
    "/" = {
      if (length(y) != 1 && length(y) != length(x)) {
        vctrs::stop_incompatible_op(
          op, x, y,
          details = paste(
            "To multiply or divide a numeric and a vector3,",
            "the numeric must be either length 1",
            "or the same length as the vector3.",
            vector3_help_message
          )
        )
      }
      new_vector3(
        x = vctrs::vec_arith_base(op, x$x, y),
        y = vctrs::vec_arith_base(op, x$y, y),
        z = vctrs::vec_arith_base(op, x$z, y)
      )
    },
    vctrs::stop_incompatible_op(op, x, y, details = vector3_help_message)
  )
}

#' @rdname vector3_arith
#' @method vec_arith.dddr_vector3 MISSING
#' @export
vec_arith.dddr_vector3.MISSING <- function(op, x, y, ...) {
  switch(
    op,
    "-" = new_vector3(x = -x$x, y = -x$y, z = -x$z),
    vctrs::stop_incompatible_op(op, x, y, details = vector3_help_message)
  )
}

#' @rdname vector3_arith
#' @importFrom vctrs vec_arith.numeric
#' @method vec_arith.numeric dddr_vector3
#' @export
vec_arith.numeric.dddr_vector3 <- function(op, x, y, ...) {
  switch(
    op,
    # commutative operations can be switched
    "+" = , # nolint
    "*" = vec_arith.dddr_vector3.numeric(op, y, x, ...),
    # negative forces upgrade
    "-" = vec_arith.dddr_vector3.dddr_vector3(
      op, upgrade_to_vector3(x), y, ...
    ),
    vctrs::stop_incompatible_op(op, x, y, details = vector3_help_message)
  )
}

#' Mathematical operations on vectors
#'
#' Some summary and cumulant functions can apply to vectors as well. In
#' particular, `mean`, `sum`, and `cumsum` have meanings when applied to
#' vector3s.
#'
#' @param .fn Mathematical function to apply
#' @param .x Argument to the function
#' @param ... Unused; reserved for extensibility
#'
#' @examples
#' mean(vector3(x = 1:4, y = 2:5, z = 3:6))
#' sum(vector3(x = 1:4, y = 2:5, z = 3:6))
#' cumsum(vector3(x = 1:4, y = 2:5, z = 3:6))
#' @name vector3_math
NULL

#' @rdname vector3_math
#' @importFrom vctrs vec_math
#' @method vec_math dddr_vector3
#' @export
vec_math.dddr_vector3 <- function(.fn, .x, ...) {
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
    stop(paste0(
      "Mathematical functio not permitted on <vector3>: `",
      .fn,
      "`. See `?vector3_math` for more information on vector mathematics."
    ))
  )
}

#' Determine the distance of a vector or between two vectors
#'
#' @param to Vector to measure the distance of
#' @param from Optional. Instead of length being calculated from the origin,
#'   it is calculated from this point instead.
#'
#' @return A numeric vector of distances
#'
#' @export
distance <- function(to, from = NULL) {
  if (!is.null(from)) {
    to <- to - from
  }
  sqrt(to$x^2 + to$y^2 + to$z^2)
}

#' Normalize vector length
#'
#' `normalize` scales vectors so that direction is preserved but length can be
#' varied. The primary usage is to convert a vector to unit length, i.e, a
#' length of 1. This function accepts a second argument, `length`, to set a
#' certain value for the magnitude of the vector.
#'
#' @param v The vectors to normalize
#' @param length (Optional) Normalized vectors will have this length. Both
#'   length-1 and length-N numerics are accepted here.
#'
#' @export
normalize <- function(v, length = 1) {
  length * v / distance(v)
}

#' Vector (cross) and scalar (dot) products
#'
#' Compute the vector and scalar products, also known as the cross and dot
#' products.
#'
#' The cross product of two vectors is a vector perpendicular to both inputs and
#' with the same length as the area of a parallelogram constructed by both
#' inputs. The dot product returns a scalar value that is the largest (and the
#' product of the lengths) when the two input vectors are aligned, smallest when
#' they are 180 degrees apart, and zero when they are perpendicular.
#'
#' @param a,b Vectors to be multiplied
#'
#' @name vector3_prod
NULL

#' @rdname vector3_prod
#' @export
cross <- function(a, b) {
  if (!inherits(b, "dddr_vector3")) {
    b <- upgrade_to_vector3(b)
  }
  new_vector3(
    x = a$y * b$z - a$z * b$y,
    y = a$z * b$x - a$x * b$z,
    z = a$x * b$y - a$y * b$x
  )
}

#' @rdname vector3_prod
#' @export
dot <- function(a, b) {
  if (!inherits(b, "dddr_vector3")) {
    b <- upgrade_to_vector3(b)
  }
  a$x * b$x + a$y * b$y + a$z * b$z
}

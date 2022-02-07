
#' Arithmetic operations on vectors
#'
#' Addition or subtraction requires two `vector3` of equal length or length 1.
#' In the case one vector is length 1, it is broadcast across the other value.
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
#' vector3(x = 1:4, y = 2:5, z = 3:6) + vector3(x = 4:7, y = 5:8, z = 6:9)
#' vector3(x = 1:4, y = 2:5, z = 3:6) - vector3(2, 0, -1)
#'
#' vector3(x = 1:4, y = 2:5, z = 3:6) * 4
#' vector3(x = 1:4, y = 2:5, z = 3:6) / c(3, 1, 4, 1)
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
    "+" = ,
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
    "*" = ,
    "/" = {
      if (length(x) != 1 && length(y) != 1 && length(y) != length(x)) {
        vctrs::stop_incompatible_op(
          op, x, y,
          details = paste(
            "To multiply or divide a numeric and a vector3,",
            "either the numeric or the vector3 must be either length 1",
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
    # multiplication is commutative and can be switched
    "*" = vec_arith.dddr_vector3.numeric(op, y, x, ...),
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
    rlang::abort(
      message = paste0(
        "Mathematical function not permitted on <vector3>: `",
        .fn,
        "`. See `?vector3_math` for more information on vector mathematics."
      ),
      class = "dddr_error_math"
    )
  )
}

#' Vector properties
#'
#' Vectors have properties such as magnitude and direction. These are easily
#' accessible through the `magnitude` and `direction` functions.
#'
#' @param v vectors
#' @param norm optional norm argument from list 'euclidean', 'manhattan', 'L1', 'L2', 'infinity'
#'
#' @name vector3_prop
NULL

#' @rdname vector3_prop
#' @export
magnitude <- function(v, norm = c("euclidean", "manhattan", "L1", "L2", "infinity")) {
  if (!inherits(v, "dddr_vector3")) {
    rlang::abort(
      message = paste0(
        "`magnitude` expects first argument to inherit from `dddr_vector3`. ",
        "Instead, the first argument was `",
        paste0(class(v), collapse = "/"),
        "`."
      ),
      class = "dddr_error_math"
    )
  }

  if (missing(norm)) {
    sqrt(v$x^2 + v$y^2 + v$z^2)
  }
  else {
    norm <- match.arg(norm)

    switch(
      norm,
      "euclidean" = sqrt(v$x^2 + v$y^2 + v$z^2),
      "manhattan" = abs(v$x) + abs(v$y) + abs(v$z),
      "L1" = abs(v$x) + abs(v$y) + abs(v$z),
      "L2" = sqrt(v$x^2 + v$y^2 + v$z^2),
      "infinity" = pmax(abs(v$x), abs(v$y), abs(v$z)),
    )
  }
}

#' @rdname vector3_prop
#' @export
direction <- function(v) {
  v / magnitude(v)
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
  if (!inherits(a, "dddr_vector3") || !inherits(b, "dddr_vector3")) {
    rlang::abort(
      message = paste0(
        "`cross` expects both arguments to inherit from `dddr_vector3`. ",
        "Instead, the arguments were `",
        paste0(class(a), collapse = "/"),
        "` and `",
        paste0(class(b), collapse = "/"),
        "`."
      ),
      class = "dddr_error_math"
    )
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
  if (!inherits(a, "dddr_vector3") || !inherits(b, "dddr_vector3")) {
    rlang::abort(
      message = paste0(
        "`dot` expects both arguments to inherit from `dddr_vector3`. ",
        "Instead, the arguments were `",
        paste0(class(a), collapse = "/"),
        "` and `",
        paste0(class(b), collapse = "/"),
        "`."
      ),
      class = "dddr_error_math"
    )
  }
  a$x * b$x + a$y * b$y + a$z * b$z
}


vec_arith.vrm_vector3 <- function(op, x, y, ...) {
  UseMethod("vec_arith.vrm_vector3", y)
}
vec_arith.vrm_vector3.default <- function(op, x, y, ...) {
  vctrs::stop_incompatible_op(op, x, y)
}

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

vec_arith.numeric.vrm_vector3 <- function(op, x, y, ...) {
  switch(
    op,
    "+" = ,
    "*" = vec_arith.vrm_vector3.numeric(op, y, x, ...),
    #"-" = vec_arith.vrm_vector3.numeric("+", y, -x, ...),
    vctrs::stop_incompatible_op(op, x, y)
  )
}

normalize <- function(v, length=1) {
  v / sqrt(v$x^2 + v$y^2 + v$z^2)
}

distance <- function(to, from=c(0, 0, 0)) {
  stop("Not Implemented Yet")
}

cross <- function(pointer, middle, handedness="settings") {
  # use settings default, or left or right.
  # TODO: how to get "handedness" to show up as settings, right, left in the documentation
  stop("Not Implemented Yet")
}

dot <- function(a, b) {
  stop("Not Implemented Yet")
}

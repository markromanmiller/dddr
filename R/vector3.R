# what's the vector ontology?
new_vector3 <- function(x=double(), y=double(), z=double()) {
  vctrs::vec_assert(x, ptype=double())
  vctrs::vec_assert(y, ptype=double())
  vctrs::vec_assert(z, ptype=double())
  vctrs::new_rcrd(list(x=x, y=y, z=z), class="vrm_vector3")
}

vector3 <- function(x, y, z) {
  l <- vctrs::vec_cast_common(x, y, z, .to=double())
  l <- vctrs::vec_recycle_common(l[[1]], l[[2]], l[[3]])
  new_vector3(l[[1]], l[[2]], l[[3]])
}


`$.vrm_vector3` <- function(v, name) {
  # should xyz conventions be a setting?
  # should all fields be accessible?
  if (name %in% c("x", "y", "z")) {
    return(vctrs::field(v, name))
  } else {
    return(NULL)
  }
}

vec_arith.vrm_vector3 <- function(op, x, y, ...) {
  UseMethod("vec_arith.vrm_vector3", y)
}
vec_arith.vrm_vector3.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
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
    vctrs::stop_incompatible_op(op, x, y)
  )
}

vec_arith.numeric.vrm_vector3 <- function(op, x, y, ...) {
  vec_arith.vrm_vector3.numeric(op, y, x, ...)
}



format.vrm_vector3 <- function(v, ...) {
  # TODO: keep width, etc in mind.
  x <- vctrs::field(v, "x")
  y <- vctrs::field(v, "y")
  z <- vctrs::field(v, "z")

  out <- paste0("(", x, ", ", y, ", ", z, ")")
  out[is.na(x) | is.na(y) | is.na(z)] <- NA

  out
}

vec_ptype_abbr.vrm_vector3 <- function(x, ...) {
  "vector3"
}

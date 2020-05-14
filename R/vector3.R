

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





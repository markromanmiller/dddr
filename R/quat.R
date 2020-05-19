
#' @export
new_quat <- function(w=double(), x=double(), y=double(), z=double()) {
  vctrs::vec_assert(w, ptype=double())
  vctrs::vec_assert(x, ptype=double())
  vctrs::vec_assert(y, ptype=double())
  vctrs::vec_assert(z, ptype=double())
  vctrs::new_rcrd(list(w=w, x=x, y=y, z=z), class="vrm_quat")
}

#' @export
quat <- function(w, x, y, z) {
  # should empty arguments be the identity quaternion?
  l <- vctrs::vec_cast_common(w, x, y, z, .to=double())
  l <- vctrs::vec_recycle_common(l[[1]], l[[2]], l[[3]], l[[4]])
  new_quat(l[[1]], l[[2]], l[[3]], l[[4]])
}

#' @export
`$.vrm_quat` <- function(q, name) {
  # should xyz conventions be a setting?
  # should all fields be accessible?
  if (name %in% c("w", "x", "y", "z")) {
    return(vctrs::field(q, name))
  } else {
    return(NULL)
  }
}

#' @export
format.vrm_quat <- function(x, ...) {
  q <- x # don't overwrite the name
  # TODO: keep width, etc in mind.

  w <- vctrs::field(q, "w")
  x <- vctrs::field(q, "x")
  y <- vctrs::field(q, "y")
  z <- vctrs::field(q, "z")

  out <- paste0("(", w, "; ", x, ", ", y, ", ", z, ")")
  out[is.na(x) | is.na(y) | is.na(z)] <- NA

  out
}

#' @export
vec_ptype_abbr.quat <- function(x, ...) {
  "quat"
}


upgrade_quat <- function(v) {
  stopifnot(length(v) == 4)

  new_quat(w = v[[1]], x = v[[2]], y = v[[3]], z = v[[4]])
}




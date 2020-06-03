
#' Internal method for creating a new quat
#'
#' This follows the new / validate / user-facing pattern recommended in the
#' `vctrs` documentation.
#'
#' @param w,x,y,z Quaternion entries, expected to be double vectors
#'
#' @keywords internal
new_quat <- function(w = double(), x = double(), y = double(), z = double()) {
  vctrs::vec_assert(w, ptype = double())
  vctrs::vec_assert(x, ptype = double())
  vctrs::vec_assert(y, ptype = double())
  vctrs::vec_assert(z, ptype = double())
  vctrs::new_rcrd(list(w = w, x = x, y = y, z = z), class = "dddr_quat")
}

#' Create a quaternion
#'
#' Creates a quaternion, a four-dimensional number that represents 3D rotation
#' well. The values underneath the quaternions are not interpretable for the
#' naive user; however, one can learn how to read these values. See the vignette
#' [TODO: link vignette] for an explanation.
#'
#' @param w,x,y,z Numeric vectors of the quaternion values. It is rare to need
#'   to access these fields.
#'
#' @export
quat <- function(w, x, y, z) {
  # should empty arguments be the identity quaternion?
  l <- vctrs::vec_cast_common(w, x, y, z, .to = double())
  l <- vctrs::vec_recycle_common(l[[1]], l[[2]], l[[3]], l[[4]])
  new_quat(l[[1]], l[[2]], l[[3]], l[[4]])
}

#' @rdname field_access
#' @export
`$.dddr_quat` <- function(x, name) {
  # should xyz conventions be a setting?
  # should all fields be accessible?
  if (name %in% c("w", "x", "y", "z")) {
    return(vctrs::field(x, name))
  } else {
    return(NULL)
  }
}

#' @export
format.dddr_quat <- function(x, ..., digits = 4) {
  q <- x # don't overwrite the name

  w <- vctrs::field(q, "w")
  x <- vctrs::field(q, "x")
  y <- vctrs::field(q, "y")
  z <- vctrs::field(q, "z")

  out <- paste0(
    "(",
    format(w, ..., digits = digits),
    "; ",
    format(x, ..., digits = digits),
    ", ",
    format(y, ..., digits = digits),
    ", ",
    format(z, ..., digits = digits), ")"
  )
  out[is.na(w) | is.na(x) | is.na(y) | is.na(z)] <- NA

  out
}

#' @importFrom vctrs vec_ptype_abbr
#' @method vec_ptype_abbr dddr_quat
#' @export
vec_ptype_abbr.dddr_quat <- function(x, ...) {
  "quat"
}


#' Quaternion helpers
#'
#' Sometimes objects are in a format that is not truly a quaternion but has
#' unambigous meaning. In those cases, we translate from the length-four numeric
#' vector to a length-one quaternion
#'
#' `upgrade` assumes a length 4 numeric, `ensure` checks if it's already a quat
#' first.
#'
#' @param q a length-4 numeric vector
#' @name quat_helpers

#' @rdname quat_helpers
#' @export
upgrade_to_quat <- function(q) {
  if (length(q) != 4) {
    rlang::abort(
      paste(
        "Cannot upgrade object to quat, as object has length",
        length(q),
        "instead of length 4"
      ),
      class = "dddr_size_quat"
    )
  }
  new_quat(w = q[[1]], x = q[[2]], y = q[[3]], z = q[[4]])
}

#' @rdname quat_helpers
#' @export
ensure_quat <- function(q) {
  if (!inherits(q, "dddr_quat")) {
    q <- upgrade_to_quat(q)
  }
  q
}

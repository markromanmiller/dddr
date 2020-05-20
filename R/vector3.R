
#' Internal method for creating a new vector3
#'
#' This follows the pattern recommended in the `vctrs` documentation.
#'
#' @param x,y,z Vector entries, expected to be double vectors
#'
#' @keywords internal
new_vector3 <- function(x=double(), y=double(), z=double()) {
  vctrs::vec_assert(x, ptype=double())
  vctrs::vec_assert(y, ptype=double())
  vctrs::vec_assert(z, ptype=double())
  vctrs::new_rcrd(list(x=x, y=y, z=z), class="vrm_vector3")
}

#' Create a 3D vector
#'
#' Creates a three-dimensional vector given three vectors representing Cartesian coordinates.
#'
#' @param x,y,z Numeric vectors representing the the vector's coordinates in the specified dimension.
#'
#' @export
vector3 <- function(x, y, z) {
  l <- vctrs::vec_cast_common(x, y, z, .to=double())
  l <- vctrs::vec_recycle_common(l[[1]], l[[2]], l[[3]])
  new_vector3(l[[1]], l[[2]], l[[3]])
}

#' Vector field access
#'
#' In order to access fields within each vector, the `$` operator is used.
#'
#' @param x Vector3 whose fields to access
#' @param name Field name to access; should be one of "x", "y", or "z"
#'
#' @name field_access
#' @examples
#' vector3(x=1:4, y=2:5, z=3:6)$y
NULL

#' @rdname field_access
#' @export
`$.vrm_vector3` <- function(x, name) {
  # should xyz conventions be a setting?
  # should all fields be accessible?
  if (name %in% c("x", "y", "z")) {
    return(vctrs::field(x, name))
  } else {
    return(NULL)
  }
}

#' @export
format.vrm_vector3 <- function(x, ...) {
  v <- x # don't overwrite the name
  # TODO: keep width, etc in mind.
  x <- vctrs::field(v, "x")
  y <- vctrs::field(v, "y")
  z <- vctrs::field(v, "z")

  out <- paste0("(", x, ", ", y, ", ", z, ")")
  out[is.na(x) | is.na(y) | is.na(z)] <- NA

  out
}

#' @importFrom vctrs vec_ptype_abbr
#' @method vec_ptype_abbr vrm_vector3
#' @export
vec_ptype_abbr.vrm_vector3 <- function(x, ...) {
  "vct3"
}

#' Internal helper function for creating a new vector3 from a length-3 numeric vector.
#'
#' @param v a length-3 numeric vector
#'
#' @keywords internal
upgrade_vector3 <- function(v) {
  stopifnot(length(v) == 3)

  new_vector3(x = v[[1]], y = v[[2]], z = v[[3]])
}

format_and_align_right <- function(x, width) {
  x <- format(x, width=width)
  extent <- pillar::get_extent(x)
  spaces <- pmax(width - extent, 0L)
  paste0(strrep(" ", spaces), x)
}

#' @export
format.vrm_vector3_pillar <- function(x, width, ...) {
  # if width is maximum, everyone gets the width they request.
  if (width >= attr(x, "width")) {
    out <- paste0(
      "(",
      format_and_align_right(x$pillar_x, width=attr(x$pillar_x, "width")),
      ", ",
      format_and_align_right(x$pillar_y, width=attr(x$pillar_y, "width")),
      ", ",
      format_and_align_right(x$pillar_z, width=attr(x$pillar_z, "width")),
      ")"
    )
  } else if (width < attr(x, "min_width")){
    stop("Width smaller than min_width")
  } else {
    # well this is ugly.
    l <- list(x$pillar_x, x$pillar_y, x$pillar_z)
    to_claim <- unlist(lapply(l, function(a) {attr(a, "width") - attr(a, "min_width")}))
    total_to_claim <- sum(to_claim)

    need_to_claim <- (width - attr(x, "min_width"))
    claim_rate <- need_to_claim / total_to_claim
    claimed <- claim_rate * to_claim

    claimed_int <- as.integer(floor(claimed))
    claimed_frac <- claimed - claimed_int

    remaining <- round(sum(claimed_frac))
    additions <- rank(claimed_frac, ties.method="random") > (3 - remaining)

    min_width <- unlist(lapply(l, function(a) {attr(a, "min_width")}))

    total_spaces <- additions + claimed_int + min_width

    out <- paste0(
      "(",
      format_and_align_right(x$pillar_x, width=total_spaces[[1]]),
      ", ",
      format_and_align_right(x$pillar_y, width=total_spaces[[2]]),
      ", ",
      format_and_align_right(x$pillar_z, width=total_spaces[[3]]),
      ")"
    )

  }

  #class(out) <- c("pillar_shaft", class(out))
  attr(out, "align") <- "right"
  out
}

#' @importFrom pillar pillar_shaft
#' @method pillar_shaft vrm_vector3
#' @export
pillar_shaft.vrm_vector3 <- function(v) {

  # format the three numbers as pillars
  pillar_x <- pillar::pillar_shaft(v$x)
  pillar_y <- pillar::pillar_shaft(v$y)
  pillar_z <- pillar::pillar_shaft(v$z)

  # the 6 represents 2 parens, 2 spaces, and 2 commas.
  width <- as.integer(sum(attr(pillar_x, "width"), attr(pillar_y, "width"), attr(pillar_z, "width"), 6))

  # specify min_width
  min_width <- as.integer(sum(attr(pillar_x, "min_width"), attr(pillar_y, "min_width"), attr(pillar_z, "min_width"), 6))

  out <- list(pillar_x=pillar_x, pillar_y=pillar_y, pillar_z=pillar_z)
  class(out) <- c("vrm_vector3_pillar", class(out))
  attr(out, "width") <- width
  attr(out, "min_width") <- min_width
  out
}



#' Internal method for creating a new vector3
#'
#' This follows the pattern recommended in the `vctrs` documentation.
#'
#' @param x,y,z Vector entries, expected to be double vectors
#'
#' @keywords internal
new_vector3 <- function(x = double(), y = double(), z = double()) {
  vctrs::vec_assert(x, ptype = double())
  vctrs::vec_assert(y, ptype = double())
  vctrs::vec_assert(z, ptype = double())
  vctrs::new_rcrd(list(x = x, y = y, z = z), class = "dddr_vector3")
}

#' Create a 3D vector
#'
#' Creates a three-dimensional vector given three vectors representing Cartesian
#' coordinates.
#'
#' @param x,y,z Numeric vectors representing the the vector's coordinates in the
#'   specified dimension.
#'
#' @export
vector3 <- function(x, y, z) {
  l <- vctrs::vec_cast_common(x, y, z, .to = double())
  l <- vctrs::vec_recycle_common(l[[1]], l[[2]], l[[3]])
  new_vector3(l[[1]], l[[2]], l[[3]])
}

#' Vector field access
#'
#' In order to access fields within each vector, the `$` operator is used.
#'
#' @param x Vector3 whose fields to access
#' @param name Field name to access. For vectors, this should be one of "x",
#'   "y", or "z". For quaternions, this can include "w" as well.
#'
#' @name field_access
#' @examples
#' vector3(x = 1:4, y = 2:5, z = 3:6)$y
NULL

#' @rdname field_access
#' @export
`$.dddr_vector3` <- function(x, name) {
  # should xyz conventions be a setting?
  # should all fields be accessible?
  if (name %in% c("x", "y", "z")) {
    return(vctrs::field(x, name))
  } else {
    return(NULL)
  }
}

#' @method is.na dddr_vector3
#' @export
is.na.dddr_vector3 <- function(x) {
  is.na(x$x) | is.na(x$y) | is.na(x$z)
}

#' @export
format.dddr_vector3 <- function(x, ..., digits = 4) {
  v <- x # don't overwrite the name

  x <- vctrs::field(v, "x")
  y <- vctrs::field(v, "y")
  z <- vctrs::field(v, "z")

  out <- paste0(
    "(",
    format(x, ..., digits = digits),
    ", ",
    format(y, ..., digits = digits),
    ", ",
    format(z, ..., digits = digits), ")"
  )

  out[is.na(x) | is.na(y) | is.na(z)] <- NA

  out
}

#' @importFrom vctrs vec_ptype_abbr
#' @method vec_ptype_abbr dddr_vector3
#' @export
vec_ptype_abbr.dddr_vector3 <- function(x, ...) {
  "vct3"
}

format_and_align_right <- function(x, width) {
  x <- format(x, width = width)
  extent <- pillar::get_extent(x)
  spaces <- pmax(width - extent, 0L)
  paste0(strrep(" ", spaces), x)
}

#' @method format dddr_vector3_pillar
#' @export
format.dddr_vector3_pillar <- function(x, width, ...) {
  # if width is maximum, everyone gets the width they request.
  if (width >= attr(x, "width")) {
    out <- paste0(
      "(",
      format_and_align_right(x$pillar_x, width = attr(x$pillar_x, "width")),
      ", ",
      format_and_align_right(x$pillar_y, width = attr(x$pillar_y, "width")),
      ", ",
      format_and_align_right(x$pillar_z, width = attr(x$pillar_z, "width")),
      ")"
    )
  } else {
    # Pillar guarantees width >= min_width

    l <- list(x$pillar_x, x$pillar_y, x$pillar_z)
    to_claim <- unlist(lapply(l, function(a) {
      attr(a, "width") - attr(a, "min_width")
    }))
    total_to_claim <- sum(to_claim)

    need_to_claim <- (width - attr(x, "min_width"))
    claim_rate <- need_to_claim / total_to_claim
    claimed <- claim_rate * to_claim

    claimed_int <- as.integer(floor(claimed))
    claimed_frac <- claimed - claimed_int

    remaining <- round(sum(claimed_frac))
    additions <- rank(claimed_frac, ties.method = "random") > (3 - remaining)

    min_width <- unlist(lapply(l, function(a) {
      attr(a, "min_width")
    }))

    total_spaces <- additions + claimed_int + min_width

    out <- paste0(
      "(",
      format_and_align_right(x$pillar_x, width = total_spaces[[1]]),
      ", ",
      format_and_align_right(x$pillar_y, width = total_spaces[[2]]),
      ", ",
      format_and_align_right(x$pillar_z, width = total_spaces[[3]]),
      ")"
    )
  }

  attr(out, "align") <- "right"
  out
}

#' @importFrom pillar pillar_shaft
#' @method pillar_shaft dddr_vector3
#' @export
pillar_shaft.dddr_vector3 <- function(v) {

  pillar_x <- pillar::pillar_shaft(v$x)
  pillar_y <- pillar::pillar_shaft(v$y)
  pillar_z <- pillar::pillar_shaft(v$z)

  width <- as.integer(sum(
    attr(pillar_x, "width"),
    attr(pillar_y, "width"),
    attr(pillar_z, "width"),
    6 # the 6 represents 2 parens, 2 spaces, and 2 commas.
  ))

  min_width <- as.integer(sum(
    attr(pillar_x, "min_width"),
    attr(pillar_y, "min_width"),
    attr(pillar_z, "min_width"),
    6 # ditto
  ))

  out <- list(pillar_x = pillar_x, pillar_y = pillar_y, pillar_z = pillar_z)
  class(out) <- c("dddr_vector3_pillar", class(out))
  attr(out, "width") <- width
  attr(out, "min_width") <- min_width
  out
}

#' @keywords internal
extract_dimension <- function(v, direction) {
  cartesian_direction <- get_semantics()[[direction]]
  axis <- substr(cartesian_direction, 2, 2)
  `$.dddr_vector3`(v, axis)
}

#' @keywords internal
extract_horizontal_dimension <- function(view) {
    # if I'm looking at the front, it means forward is pointing towards me.
    # Up is on the top, as usual.
    # I need another function to call the right extract functions.
  switch(view,
    "at front" = "left", # positive X is to the left of the object.
    "at back" = "right",
    "at top" = "left",
    "at bottom" = "left",
    #"at left", "" conventions...
    NA
  )
}

#' @keywords internal
extract_vertical_dimension <- function(view) {
  switch(view,
         "at front" = "up",
         "at back" = "up",
         "at top" = "backward",
         "at bottom" = "forward",
         NA
  )
}

#' @keywords internal
extract_horizontal <- function(v) {
  dimension <- extract_horizontal_dimension(attr(v, "view"))
  extract_dimension(v, dimension)
}

#' @keywords internal
extract_vertical <- function(v) {
  dimension <- extract_vertical_dimension(attr(v, "view"))
  extract_dimension(v, dimension)
}

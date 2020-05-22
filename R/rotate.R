

#' Rotate vectors and quaternions
#'
#' Rotate points or quaternions using a handful of ways to define a rotation.
#'
#' @param rotand Object to be rotated; can be either a vector or a quaternion
#' @param ... Additional arguments passed on to underlying S3 methods
#' @param rotator (Optional) Quaternion specifying the rotation to perform. If
#'   this argument is not specified, it is constructed using the others.
#' @param origin (Optional) The origin of the rotation, i.e, the point that
#'   should not change positions during the rotation.
#' @param axis (Optional) The axis of rotation
#' @param angle (Optional) The angle of rotation, specified in radians. If this
#'   is not provided, it is calculated using `from` and `to`
#' @param from,to (Optional) Instead of specifying an axis-angle pair, or angle
#'   amount, a rotation is performed mapping the direction of `from` to the
#'   direction of `to`.
#'
#' @examples
#' rotate(vector3(x=1:4, y=2:5, z=3:6), rotator=quat(0,1,0,0))
#' rotate(vector3(x=1:4, y=2:5, z=3:6), rotator=quat(0,1,0,0), origin=c(1,2,3))
#' \dontrun{ # not implemented yet
#'   rotate(vector3(x=1:4, y=2:5, z=3:6), axis=c(1,0,0), angle=pi/4)
#'   rotate(vector3(x=1:4, y=2:5, z=3:6), from=c(1,0,0), to=c(1,0,0))
#' }
#'
#' @name rotation
NULL

#' @rdname rotation
#' @export
rotate <- function(rotand, ...) {
  UseMethod("rotate", rotand)
}

#' @rdname rotation
#' @method rotate dddr_vector3
#' @export
rotate.dddr_vector3 <- function(rotand, ...) {
  rotate.dddr_object(rotand, ...)
}

#' @rdname rotation
#' @method rotate dddr_quat
#' @export
rotate.dddr_quat <- function(rotand, ...) {
  rotate.dddr_object(rotand, ...)
}

#' @rdname rotation
#' @export
rotate.dddr_object <- function(rotand, rotator=NULL, origin=c(0,0,0), axis=NULL, angle=NULL, from=NULL, to=NULL, ...) {
  # TODO: what order makes the most sense? what would make the most sense without context?
  if (is.null(rotator)) {
    # try to make the rotator.
    if (is.null(angle)) {
      stop("from/to not implemented yet")
    }
    if (!is.null(from) || !is.null(to)) {
      stop("cannot accept both angle and from/to as arguments")
    }

    # upgrade + normalize axis
    if(!inherits(axis, "dddr_vector3")) {
      # assume it's a nuermic vector, if not, whine.
      axis <- normalize(upgrade_to_vector3(axis))
    }

    # TODO: refactor to a separate axis / angle constructor?
    rotator <- quat(
      w = cos(angle / 2),
      x = axis$x * sin(angle/2),
      y = axis$y * sin(angle/2),
      z = axis$z * sin(angle/2)
    )
  }

  rotand_was_vector <- FALSE

  if (inherits(rotand, "dddr_vector3")) {
    rotand_was_vector <- TRUE
    rotand <- quat(
      w = 0,
      x = rotand$x,
      y = rotand$y,
      z = rotand$z
    )
  }

  out <- rotator * rotand * Conj(rotator)

  if (rotand_was_vector) {
    out <- vector3(
      x = out$x,
      y = out$y,
      z = out$z
    )
  }

  out
}


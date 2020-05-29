

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
#' example_vector <- vector3(x = 1:4, y = 2:5, z = 3:6)
#' rotate(example_vector, rotator = quat(0, 1, 0, 0))
#' rotate(example_vector, rotator = quat(0, 1, 0, 0), origin = c(1, 2, 3))
#' rotate(example_vector, axis = c(1, 0, 0), angle = pi / 4)
#' rotate(example_vector, from = c(1, 0, 0), to = c(1, 0, 0))
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
  rotate_dddr(rotand, ...)
}

#' @rdname rotation
#' @method rotate dddr_quat
#' @export
rotate.dddr_quat <- function(rotand, ...) {
  rotate_dddr(rotand, ...)
}

#' @rdname rotation
#' @export
rotate_dddr <- function(
  rotand, rotator = NULL, origin = c(0, 0, 0),
  axis = NULL, angle = NULL, from = NULL, to = NULL
) {
  if (is.null(rotator)) {
    # try to make the rotator.
    if (is.null(axis)) {
      from <- ensure_vector3(from)
      to <- ensure_vector3(to)
      axis <- cross(from, to)
    } else if (is.null(angle)) {
      # Axis was specified, angle was not. Generate "angle" by projecting
      # from/to onto the plane normal to axis, and then compute the angle
      # between (done below)
      from <- ensure_vector3(from)
      to <- ensure_vector3(to)
      axis <- ensure_vector3(axis)
      from <- reject(from, axis)
      to <- reject(to, axis)
    } else {
      # quick check
      if (!is.null(from) && !is.null(angle) && !is.null(to)) {
        # axis, angle, from, and to are all not null
        # that's weird and shouldn't happen.
        warning(paste("The parameters axis, angle, from, and to are all",
                      "specified. Some are redundant."))
      }
    }

    if (is.null(angle)) {
      # it matters if the axis was created or not.
      angle <- angle_between(from, to)
      # if it was created, this is all we need.
      # if not, we need to project it.
    }

    # upgrade + normalize axis
    axis <- ensure_vector3(axis)
    axis <- normalize(axis)

    # TODO: refactor to a separate axis / angle constructor?
    rotator <- quat(
      w = cos(angle / 2),
      x = axis$x * sin(angle / 2),
      y = axis$y * sin(angle / 2),
      z = axis$z * sin(angle / 2)
    )
  } else {
    if (!is.null(from) || !is.null(to) || !is.null(axis) || !is.null(angle)) {
      warning("Argument `rotator` precedes any other rotation arguments.")
    }
  }

  rotand_was_vector <- FALSE

  if (inherits(rotand, "dddr_vector3")) {
    rotand_was_vector <- TRUE
    # subtract origin here

    if (!inherits(origin, "dddr_vector3")) {
      origin <- upgrade_to_vector3(origin)
    }
    rotatable <- rotand - origin
    rotand <- quat(
      w = 0,
      x = rotatable$x,
      y = rotatable$y,
      z = rotatable$z
    )
  } else if (!all.equal(origin, c(0, 0, 0))) {
    warning(paste(
      "Argument `origin` does not apply to rotation of a",
      "quaternion. Ignoring argument `origin`."))
  }

  out <- rotator * rotand * Conj(rotator)

  if (rotand_was_vector) {
    out <- vector3(
      x = out$x,
      y = out$y,
      z = out$z
    ) + origin
  }

  out
}

#' Euler angles
#'
#' Extract euler angles from a quaternion. These follow Unity's conventions of
#' yaw around Y, pitch around X, and roll around Z. They are applied in order of
#' roll-pitch-yaw.
#'
#' @param q Quaternions to extract angles from
#'
#' @return Angle in radians
#' @name euler_angles
NULL


#' @rdname euler_angles
#' @export
yaw <- function(q) {
  # make z
  z <- rotate(vector3(0, 0, 1), q)
  # use x and z in atan2.
  atan2(z$x, z$z)
}

#' @rdname euler_angles
#' @export
pitch <- function(q) {
  z <- rotate(vector3(0, 0, 1), q)
  atan2(-z$y, sqrt(z$z^2 + z$x^2))
}

#' @rdname euler_angles
#' @export
roll <- function(q) {
  x <- rotate(vector3(1, 0, 0), q)
  y <- rotate(vector3(0, 1, 0), q)
  atan2(x$y, y$y)
}

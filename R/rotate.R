
#' Internal rotation function
#' @keywords internal
make_rotator <- function(axis, angle, from, to) {
  # try to make the rotator.
  if (is.null(axis)) {
    axis <- cross(from, to)
  } else if (is.null(angle)) {
    # Axis was specified, angle was not. Generate "angle" by projecting
    # from/to onto the plane normal to axis, and then compute the angle
    # between (done below)
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

  axis <- direction(axis)

  quat(
    w = cos(angle / 2),
    x = axis$x * sin(angle / 2),
    y = axis$y * sin(angle / 2),
    z = axis$z * sin(angle / 2)
  )
}


#' Rotate vectors and quaternions
#'
#' Rotate points or quaternions using a handful of ways to define a rotation.
#'
#' @param rotand Object to be rotated; can be either a vector or a quaternion
#' @param ... Additional arguments passed on to underlying S3 methods
#' @param rotator (Optional) Quaternion specifying the rotation to perform. If
#'   this argument is not specified, it is constructed using the others.
#' @param origin (Optional) The origin of the rotation, i.e, the point that
#'   should not change position during the rotation.
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
#' rotate(example_vector, rotator = quat(0, 1, 0, 0), origin = vector3(1, 2, 3))
#' rotate(example_vector, axis = vector3(1, 0, 0), angle = pi / 4)
#' rotate(example_vector, from = vector3(1, 0, 0), to = vector3(0, 1, 0))
#' @name rotation
NULL

#' @rdname rotation
#' @export
rotate <- function(rotand, rotator=NULL, ...) {
  UseMethod("rotate", rotand)
}

#' @rdname rotation
#' @method rotate dddr_vector3
#' @export
rotate.dddr_vector3 <- function(rotand, rotator=NULL, ..., origin=NULL) {
  if (!is.null(origin)) {
    rotand <- rotand - origin
  }
  rotand <- quat(0, rotand$x, rotand$y, rotand$z)
  result <- rotate.dddr_quat(rotand, rotator, ...)
  vec3_result <- vector3(result$x, result$y, result$z)
  if (!is.null(origin)) {
    vec3_result <- vec3_result + origin
  }
  vec3_result
}

#' @rdname rotation
#' @method rotate dddr_quat
#' @export
rotate.dddr_quat <- function(
  rotand, rotator = NULL, ...,
  axis = NULL, angle = NULL, from = NULL, to = NULL
) {
  if (is.null(rotator)) {
    rotator <- make_rotator(axis, angle, from, to)
  } else {
    if (!is.null(from) || !is.null(to) || !is.null(axis) || !is.null(angle)) {
      warning(
        "Argument `rotator` takes precedence over any other rotation arguments."
        )
    }
  }
  rotator * rotand * Conj(rotator)
}

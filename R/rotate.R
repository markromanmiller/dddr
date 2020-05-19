
#' @export
rotate <- function(rotand, ...) {
  UseMethod("rotate", rotand)
}

#' @method rotate vrm_vector3
#' @export
rotate.vrm_vector3 <- function(rotand, ...) {
  rotate.vrm_object(rotand, ...)
}

#' @method rotate vrm_quat
#' @export
rotate.vrm_quat <- function(rotand, ...) {
  rotate.vrm_object(rotand, ...)
}

#' @export
rotate.vrm_object <- function(rotand, rotator=NULL, origin=c(0,0,0), axis=NULL, angle="NULL", from=NULL, to=NULL, ...) {
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
    if(!inherits(axis, "vrm_vector3")) {
      # assume it's a nuermic vector, if not, whine.
      axis <- normalize(upgrade_vector3(axis))
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

  if (inherits(rotand, "vrm_vector3")) {
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


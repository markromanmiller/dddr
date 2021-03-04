
#' Cacluate angle between vectors
#'
#' Measure the angle between two points `a` and `b`, with optional argument
#' `origin` to measure the angle around.
#'
#' @param a,b Points to measure the angle between
#' @param origin (Optional) point to measure the rotation around
#'
#' @return Angle between `a` and `b` in terms of radians
#' @export
angle_between <- function(a, b, origin = vector3(0, 0, 0)) {
  # angle between is dot divided by length.
  a <- a - origin
  b <- b - origin
  acos(dot(a, b) / (magnitude(a) * magnitude(b)))
}

#' Cacluate distance between vectors
#'
#' Measure the distance between two points `from` and `to`
#'
#' @param from,to Endpoints of the distance to be calculated
#' @export
distance_between <- function(from, to) {
  magnitude(from - to)
}

#' Vector projection and rejection
#'
#' Oftentimes it is useful to constrain a vector to lie on a line or a plane.
#' Vector projection and rejection is able to do these operations. Projection
#' reduces all vectors to lie on the line of the vector given, and rejection
#' reduces all vectors to lie on the place normal the the specified vector.
#'
#' @param x Vectors to project or reject
#' @param onto,from The vector to project onto or reject from
#' @name vecjection
NULL

#' @rdname vecjection
#' @export
project <- function(x, onto) {
  # random thought: if speed is an issue, it may make sense to add classes to
  # the vector3s saying if they've already been normalized, etc.
  (dot(x, onto) / dot(onto, onto)) * onto
}

#' @rdname vecjection
#' @export
reject <- function(x, from) {
  x - project(x, from)
}

#' Extend a point and direction towards a target plane.
#'
#' Starting at `initial`, go in `direction` until you hit the target plane
#' defined by `target_point` and `target_normal`. If you want to include vectors
#' going backwards, specify `include_backwards=TRUE`.
#'
#' This function is intended to be used in conjuction with a plot e.g., to show
#' where a peron was looking or where an object was pointing over the course of
#' time or sessions.
#'
#' @param initial Starting point for the ray
#' @param direction Direction from which to cast the ray
#' @param target_normal,target_point Define the plane that will catch the rays
#' @param include_backwards Whether to include rays that travel backwards from
#'   `initial` along `direction` to the target plane.
extend <- function(
  initial, direction, target_normal,
  target_point = c(0, 0, 0), include_backwards = F
) {
  stop("Not Implemented Yet")
}

count_non_null <- function(x) {
  sum(!sapply(x, is.null))
}

#' @export
reinterpret <- function(
  vq,
  new_x_from = NULL, new_y_from = NULL, new_z_from = NULL,
  old_x_to = NULL, old_y_to = NULL, old_z_to = NULL
) {
  # TODO: add entry for "transform" permittied can be "proper" aka proper rigid,
  # "rigid" (lengths, angles between things, allowing rotations), "similar",
  # (anlges, lengths are scaled, rotations are still meaningful), "affine" /
  # "free" (everything goes) right now, only handle

  # TODO: Does it make sense to rewrite this using more explicit matrix
  # formulations? -> it does if we're allowing more matrix-like transformations

  old_args <- list(x = old_x_to, y = old_y_to, z = old_z_to)
  new_args <- list(x = new_x_from, y = new_y_from, z = new_z_from)

  old_nns <- count_non_null(old_args)
  new_nns <- count_non_null(new_args)

  if (old_nns > 0 && new_nns > 0) {
    # TODO: change error class
    stop("checking both old and new is not implemented yet")
  }

  if (old_nns >= 2) {
    formed_args <- old_args
    formed_nns <- old_nns
    arg_source <- "old"
  } else if (new_nns >= 2) {
    formed_args <- new_args
    formed_nns <- new_nns
    arg_source <- "new"
  } else {
    stop("Not enough points to reinterpret upon")
  }

  # verify individual lengths
  for (dimension in c("x", "y", "z")) {
    arg <- formed_args[[dimension]]
    if (!(is.null(arg) || isTRUE(all.equal(magnitude(arg), 1)))) {
      stop(paste(
        "Entries in ", arg_source, dimension, "do not have magnitude 1"
      ))
    }
  }

  if (formed_nns < 3) {
    # create the cross prod
    # and verify its length
    null_dim <- names(which(sapply(formed_args, is.null)))

    replacement <- switch(null_dim,
        x = cross(formed_args[["y"]], formed_args[["z"]]),
        y = cross(formed_args[["z"]], formed_args[["x"]]),
        z = cross(formed_args[["x"]], formed_args[["y"]])
      )

    if (!isTRUE(all.equal(magnitude(replacement), 1))) {
      stop(paste("Given entries were not orthogonal."))
    }

    formed_args[[null_dim]] <- replacement
  } else {  # formed_nns is equal to 3
    # check each given pair for orthogonality
    for (pair in list(c("x", "y"), c("y", "z"), c("x", "z"))) {
      if (!isTRUE(all.equal(
        dot(
          formed_args[[pair[[1]]]],
          formed_args[[pair[[2]]]]
        ),
        0
      ))) {
        stop(paste0(
          "Entries ", arg_source, "_", pair[[1]], " and ",
          arg_source, "_", pair[[2]], " are not orthogonal."
        ))
      }
    }
  }

  # now, finally, compute the vectors in the new basis.
  if (arg_source == "old") {
    out <- vq$x * formed_args$x + vq$y * formed_args$y + vq$z * formed_args$z
  } else {
    # new_x_from, etc.
    out <- vector3(
      dot(vq, formed_args$x),
      dot(vq, formed_args$y),
      dot(vq, formed_args$z)
    )
  }
  return(out)
}

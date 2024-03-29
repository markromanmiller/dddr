
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
  eps <- .Machine$double.eps * 16
  ccos <- dot(a, b) / (magnitude(a, "L2") * magnitude(b, "L2"))
  trimmed_cos <- ifelse(
    (ccos > 1 & ccos - 1 < eps) |
      (ccos < -1 & ccos + 1 > eps),
    sign(ccos),
    ccos
  )
  acos(trimmed_cos)
}

#' Calculate distance between vectors
#'
#' Measure the distance between two points `from` and `to`
#'
#' @param from,to Endpoints of the distance to be calculated
#' @param norm optional norm argument from list 'euclidean', 'manhattan', 'L1', 'L2', 'infinity'
#'
#' @export
distance_between <- function(from, to, norm) {
  magnitude(from - to, norm)
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

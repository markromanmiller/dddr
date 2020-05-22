
#' Cacluate angle between vectors
#'
#' Measure the angle between two points `a` and `b`, with optional argument
#' `origin` to measure the angle around.
#'
#' @param a,b Points to measure the angle between
#' @param origin (Optional) point to measure the rotation around
#'
#' @return Angle between `a` and `b` in terms of radians
angle_between <- function(a, b, origin=c(0, 0, 0)) {
  # angle between is dot divided by length.
  a <- a - origin
  b <- b - origin
  acos(dot(a, b) / (distance(a) * distance(b)))
}


project <- function(points, onto){
  stop("Not Implemented Yet")
}

reject <- function(points, from) {
  stop("Not Implemented Yet")
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
#' @param include_backwards Whether to include rays that travel backwards from `initial` along `direction` to the target plane.
extend <- function(initial, direction, target_normal, target_point=c(0,0,0), include_backwards=F) {
  stop("Not Implemented Yet")
}

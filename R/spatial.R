
angle_between <- function(a, b, origin=c(0, 0, 0)) {
  stop("Not Implemented Yet")
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

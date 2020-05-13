

project <- function(points, onto){
  stop("Not Implemented Yet")
}

reject <- function(points, from) {
  stop("Not Implemented Yet")
}

#' extend a point and direction towards a target plane.
#'  starting at initinal, go in direction until you hit the target plane defined by position and normal vector
#'  if you want to include vectors traversing backwards, say T, otherwise we get NAs.
extend <- function(initial, direction, target_normal, target_point=c(0,0,0), include_backwards=F) {
  stop("Not Implemented Yet")
}



#TODO: create registration functions (vector3, quat)
# foo = vector3(px=foo_px, py=foo_py, pz=foo_pz)

#' Add together two 3-vectors
#'
#' @param a,b Collations or length-3 literals
#' @return The vector sum of \code{a} and \code{b}
#'
`+.collation` <- function(a, b) {
  # TODO: change this operation from collation to vrm_vector
  a <- eval_collation(a)
  b <- eval_collation(b)

  # should i refer to the settings (px py, pz) for this here?
  out <- list(list(
    px=a$px+b$px,
    py=a$py+b$py,
    pz=a$pz+b$pz
  ))
  class(out) <- c("eval_collation_list", class(out)) # it needs to be length 1 otherwise dplyr complains.
  out
}

# TODO: is there a unary minus function?
`-.vrm_vector3` <- function(minuend, subtrahend) {
  stop("Not Implemented Yet")
}

`*.vrm_quat` <- function(quat_a, quat_b) {
  stop("Not Implemented Yet")
}

`Conj.vrm_quat` <- function(quat) {
  stop("Not Implemented Yet")
}

cross <- function(pointer, middle, handedness="right") {
  # use settings default
  # TODO: how to get "handedness" to show up as options?
  stop("Not Implemented Yet")
}

dot <- function(a, b) {

  stop("Not Implemented Yet")
}


angle_between <- function(a, b, origin=c(0, 0, 0)) {
  stop("Not Implemented Yet")
}

normalize <- function(points, length=1) {
  stop("Not Implemented Yet")
}

distance <- function(to, from=c(0,0,0)) {
  stop("Not Implemented Yet")
}

# get yaw, pitch, and roll


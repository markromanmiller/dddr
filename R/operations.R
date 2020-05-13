
#' Add together two 3-vectors
#'
#' @param a,b Collations or length-3 literals
#' @return The vector sum of \code{a} and \code{b}
#'
`+.collation` <- function(a, b) {
  a <- eval_collation(a)
  b <- eval_collation(b)

  # should i refer to the settings (px py, pz) for this here?
  out <- list(list(
    px=a$px+b$px,
    py=a$py+b$py,
    pz=a$pz+b$pz
  ))
  class(out) <- c("eval_collation_list", "collation", class(out)) # it needs to be length 1 otherwise dplyr complains.
  out
}


`-.vrm_vector3` <- function(minuend, subtrahend) {
  # from OPS documentation: when a unary operator is encountered the Ops method is called with one argument and e2 is missing.
  stop("Not Implemented Yet")
}

`*.vrm_quat` <- function(quat_a, quat_b) {
  stop("Not Implemented Yet")
}

`Conj.vrm_quat` <- function(quat) {
  stop("Not Implemented Yet")
}

cross <- function(pointer, middle, handedness="settings") {
  # use settings default, or left or right.
  # TODO: how to get "handedness" to show up as settings, right, left in the documentation
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

distance <- function(to, from=c(0, 0, 0)) {
  stop("Not Implemented Yet")
}

# get yaw, pitch, and roll


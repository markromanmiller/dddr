
rotate.vrm_vector3(...) {
  rotate.vrm_(...)
}

rotate.vrm_quat(...) {
  rotate.vrm_(...)
}

#' rotand = dividend / addend
#'
#'
rotate.vrm_(rotand, rotator=NULL, origin=c(0,0,0), axis=NULL, angle=NULL, from=NULL, to=NULL) {
  # TODO: what if there's a second from-to option, constraining two axes?
  # TODO: What order makes the most sense?
  # signatures:
  # rotator
  # axis + angle
  # axis + from + to
  # from + to
  stop("Not Implemented Yet")
}

# TODO: change this operation from collation to vector3 / gvector / svector / vrm_vector
`+.collation` <- function(a, b) {
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


#TODO: create registration functions (vector3, quat)
# foo = vector3(px=foo_px, py=foo_py, pz=foo_pz)


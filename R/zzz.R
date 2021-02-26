# nocov start
.onLoad <- function(libname, pkgname) {

  # Load the options for dddr
  op <- options()
  op.dddr <- list(
    dddr.convention = "none"
  )
  toset <- !(names(op.dddr) %in% names(op))
  if (any(toset)) options(op.dddr[toset])

  # Load the suggested dependencies for s3 methods
  vctrs::s3_register("waldo::compare_proxy", "dddr_quat")

  invisible()
}
# nocov end

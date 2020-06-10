.onLoad <- function(libname, pkgname) {
  # This function is not automatically tested with covr
  op <- options()
  op.dddr <- list(
    dddr.convention = "none"
  )
  toset <- !(names(op.dddr) %in% names(op))
  if (any(toset)) options(op.dddr[toset])

  invisible()
}

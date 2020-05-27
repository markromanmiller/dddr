.onAttach <- function(libname, pkgname) {
  if (options("dddr.convention") == "default")
  packageStartupMessage(paste0(
    "\nUsing default 3D conventions. Please verify these conventions accurately\n",
    "describe your data. See `vignette(\"conventions\", package = \"dddr\")`\n",
    "for more information and other convention options.\n",
    "\n",
    "Run `options(dddr.convention=\"unity\")` if these conventions are\n",
    "verified to work, or run `options(dddr.convention=\"none\")` to receive\n",
    "an error when a convention is queried.\n"
  ))
}

.onLoad <- function(libname, pkgname) {
  # This function is not automatically tested with covr
  op <- options()
  op.dddr <- list(
    dddr.convention = "default"
  )
  toset <- !(names(op.dddr) %in% names(op))
  if(any(toset)) options(op.dddr[toset])

  invisible()
}

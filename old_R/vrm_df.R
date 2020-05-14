

as_vrm_df <- function(x) {
  # register it as a vrm_df
  # TODO: what's the short and snazzy code to add a class?
  class(x) <- c("vrm_df", "collation_df", class(x))
  attr(x, "collations") <- list()
  x
}

vector3 <- function(...) {
  # names of arguments are used to build
  # spec settings
  # also make sure you get the column names
  syms <- as.list(match.call(expand.dots = FALSE))$`...`
  v <- list(...)
  class(v) <- c("vector3", class(v))
  out <- list(v)
  class(out) <- c("unlisted_eval_collation", "eval_collation_list", "collation", class(out))
  attr(out, "syms") <- syms
  out
}

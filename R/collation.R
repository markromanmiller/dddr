
eval_collation <- function(collation) {
  #TODO: assert that this is indeed a collation
  source_df <- attr(collation, "source_df")
  out <- list()
  for (nm in names(collation)) {
    out[[nm]] <- rlang::eval_tidy(collation[[nm]], source_df)
  }
  class(out) <- c("eval_collation", class(out))
  out
}

collate.vrm_df <- function(vrm_df, ...) {
  # register this collation in the

  # TODO: if it's unnamed one, look in 'settings' and ensure all those suffixes match.
  args <- as.list(match.call(expand.dots = FALSE))$`...`
  n_args <- length(args)

  for (i in seq_along(args)) {
    a <- args[[i]]

    attr(a, "class") <- c("collation", attr(a, "class"))
    attr(a, "source_df") <- vrm_df

    attr(vrm_df, "collations")[[ names(args)[[i]] ]] <- a
  }
  vrm_df
}

collate <- function(...) {
  UseMethod("collate")
}

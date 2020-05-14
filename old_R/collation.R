
eval_collation <- function(collation) {
  #TODO: assert that this is indeed a collation,
  # if not, return itself (assume it's an eval_collation_list')

  if (inherits(collation, "eval_collation_list")) {
    out <- collation[[1]] # it's a list packed twice
  }
  else if (inherits(collation, "collation")) {

    source_df <- attr(collation, "source_df")
    out <- list()
    for (nm in names(collation)) {
      out[[nm]] <- rlang::eval_tidy(collation[[nm]], source_df)
    }
    class(out) <- c("eval_collation", class(out))
  }
  out
}

mutate.collation_df <- function(.data, ...) {
  # do an ugly fake masking of the calling environment... oi.
  # where else might that be necessary? in summarize and group by? that would be nice...
  envir <- rlang::caller_env()
  envir[[".collation_df"]] <- .data
  collations <- attr(.data, "collations")
  for (i in seq_along(collations)) {
    # TODO: save masked objects
    envir[[names(collations)[[i]] ]] <- collations[[i]]
  }

  NextMethod("mutate", .data, ...)
  # TODO: reinstall masked objects
}

collate.collation_df <- function(collation_df, ...) {
  # register this collation in the

  # TODO: if it's unnamed one, look in 'settings' and ensure all those prefixes / suffixes match.
  args <- as.list(match.call(expand.dots = FALSE))$`...`
  print(args)

  for (i in seq_along(args)) {
    a <- args[[i]]

    attr(a, "class") <- c("collation", attr(a, "class"))
    attr(a, "source_df") <- collation_df

    attr(collation_df, "collations")[[ names(args)[[i]] ]] <- a
  }
  collation_df
}

dplyr_col_modify.collation_df <- function(data, cols) {
  for (assigned_name in names(cols)) {
    value <- cols[[assigned_name]]

    if(inherits(value, "eval_collation_list")) {
      if(inherits(value, "unlisted_eval_collation")) {
        # add in the collation basically
        # ( how to know the spec names )
        syms <- attr(value, "syms")
        list(syms)

        attr(syms, "class") <- c("collation", attr(syms, "class"))
        attr(syms, "source_df") <- data

        attr(data, "collations")[[ assigned_name ]] <- syms
      }

      result <- cols[[assigned_name]][[1]] # unpack the doubly-packed list

      for (nm in names(result)) {
        cols[[paste(assigned_name, nm, sep="_")]] <- result[[nm]]
      }
      # delete the entry
      cols[[assigned_name]] <- NULL
    }
  }
  NextMethod(cols)
}


collate <- function(...) {
  UseMethod("collate")
}

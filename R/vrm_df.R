

as_vrm_df <- function(x) {
  # register it as a vrm_df
  # TODO: what's the short and snazzy code to add a class?
  attr(x, "class") <- c("vrm_df", attr(x, "class"))
  attr(x, "collations") <- list()
  x
}

mutate.vrm_df <- function(.data, ...) {
  # do an ugly fake masking of the calling environment... oi.
  # where else might that be necessary? in summarize and group by? that would be nice...
  envir <- rlang::caller_env()
  collations <- attr(.data, "collations")
  for (i in seq_along(collations)) {
    # TODO: save masked objects
    envir[[names(collations)[[i]] ]] <- collations[[i]]
  }

  NextMethod("mutate", .data, ...)
  # TODO: reinstall masked objects
}

dplyr_col_modify.vrm_df <- function(data, cols) {
  print(names(cols))
  for (assigned_name in names(cols)) {
    if(inherits(cols[[assigned_name]], "eval_collation_list")) {
      result <- cols[[assigned_name]][[1]] # unpack the doubly-packed list
      print(paste("assigned name: ", assigned_name))

      for (nm in names(result)) {
        print(nm)
        cols[[paste(assigned_name, nm, sep="_")]] <- result[[nm]]
      }
      # delete the entry
      cols[[assigned_name]] <- NULL
    }
  }
  NextMethod(cols)
}

# this might be the right way but I am confused...

library(tidyverse)

foobar <- tribble(~foo, ~bar, 1, 2, 3, 4)

attr(foobar, "class") <- c("vrm_df", attr(foobar, "class"))


# try modifying names,

dplyr_col_modify.vrm_df <- function(data, cols) {
  print("VRM VRM")

  # Must be implemented from first principles to avoiding edge cases in
  # [.data.frame and [.tibble (2.1.3 and earlier).

  # Apply tidyverse recycling rules
  cols <- vec_recycle_common(!!!cols, .size = nrow(data))

  out <- vec_data(data)
  attr(out, "row.names") <- .row_names_info(data, 0L)

  nms <- as_utf8_character(names2(cols))
  names(out) <- as_utf8_character(names2(out))

  for (i in seq_along(cols)) {
    nm <- nms[[i]]
    out[[nm]] <- cols[[i]]
  }

  dplyr_reconstruct(out, data)
}

tbl_subset_col <- function(x, j, j_arg) {
  if (is_null(j)) return(x)
  j <- vectbl_as_col_index(j, x, j_arg = j_arg)
  xo <- .subset(x, j)
  xo <- set_repaired_names(xo, .name_repair = "minimal")
  set_tibble_class(xo, nrow = fast_nrow(x))
}

`[.vrm_df` <- function(x, i, j, drop = FALSE, ...) {
  print("foo")
  i_arg <- substitute(i)
  j_arg <- substitute(j)

  if (missing(i)) {
    i <- NULL
    i_arg <- NULL
  } else if (is.null(i)) {
    i <- integer()
  }

  if (missing(j)) {
    j <- NULL
    j_arg <- NULL
  } else if (is.null(j)) {
    j <- integer()
  }

  # Ignore drop as an argument for counting
  n_real_args <- nargs() - !missing(drop)

  # Column or matrix subsetting if nargs() == 2L
  if (n_real_args <= 2L) {
    if (!missing(drop)) {
      warn("`drop` argument ignored for subsetting a tibble with `x[j]`, it has an effect only for `x[i, j]`.")
      drop <- FALSE
    }

    j <- i
    i <- NULL
    j_arg <- i_arg
    i_arg <- NULL

    # Special case, returns a vector:
    if (is.matrix(j)) {
      return(tbl_subset_matrix(x, j, j_arg))
    }
  }

  # From here on, i, j and drop contain correct values:
  xo <- tbl_subset_col(x, j = j, j_arg)

  if (!is.null(i)) {
    xo <- tbl_subset_row(xo, i = i, i_arg)
  }

  if (drop && length(xo) == 1L) {
    tbl_subset2(xo, 1L, j_arg)
  } else {
    vectbl_restore(xo, x)
  }
}

`[.vrm_df` <- function(x, i, j, drop=FALSE, ...) {
  print("foo")
  print(i)
  NextMethod(x, i, j, drop=drop, ...)
}

`names<-.vrm_df` <- function(...) {
  print("names<-")
  NextMethod(...)
}

`names.vrm_df` <- function(...) {
  print("names")
  NextMethod(...)
}


boink <- 6

# we want the special reference for boink to be always 7.
foobar %>% mutate(baz = khoi)






# Helper functions for tidying data

pull_bundle <- function(spec) {
  m <- regmatches(spec, regexpr("\\{[a-zA-Z]+\\}", spec), invert = NA)[[1]]
  switch(m[[2]],
         "{v}" = call(
           "vector3",
           sym(paste0(m[1], "x", m[3])),
           sym(paste0(m[1], "y", m[3])),
           sym(paste0(m[1], "z", m[3]))
         ),
         "{ed}" = call(
           "tait_bryan",
           # TODO: make this apply to axis not necessarily the yxz unity conventions.
           sym(paste0(m[1], "y", m[3])), # y for yaw
           sym(paste0(m[1], "x", m[3])), # x for pitch
           sym(paste0(m[1], "z", m[3])), # z for roll
           "degrees"
         ),

         # TODO: add in other values too.
         {
           warning("unsupported bundle spec")
           NULL
         }
  )
}

#' Bundle columns into a vector or quaternion
#'
#'
#' @export
bundle <- function(df, ..., .keep = "unused") {
  bv_args <- list(...)
  result_list <- lapply(bv_args, pull_bundle)
  names(result_list) <- names(bv_args)
  mutate(
    df,
    !!!result_list,
    .keep = .keep
  )
}

unbundle <- function(df, ..., .keep = "unused") {
  # opposite syntax of bundle
}

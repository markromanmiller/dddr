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

pull_unbundle <- function(name, enquo_val) {
  m <- regmatches(name, regexpr("\\{[a-zA-Z]+\\}", name), invert = NA)[[1]]

  # this ought to be a string, let's start with V first.
  switch(
    m[[2]],
    "{v}" = {
      res <- list(
        call("$", eval(enquo_val), "x"),
        call("$", eval(enquo_val), "y"),
        call("$", eval(enquo_val), "z")
      )
      names(res) <- paste0(m[1], c("x", "y", "z"), m[3])
      res
    },
    "{ed}" = {
      res <- list(
        call("*", call("pitch", eval(enquo_val)), 180/pi),
        call("*", call("yaw", eval(enquo_val)), 180/pi),
        call("*", call("roll", eval(enquo_val)), 180/pi)
      )
      names(res) <- paste0(m[1], c("x", "y", "z"), m[3])
      res
    },
    "{q}" = {
      res <- list(
        call("$", eval(enquo_val), "w"),
        call("$", eval(enquo_val), "x"),
        call("$", eval(enquo_val), "y"),
        call("$", eval(enquo_val), "z")
      )
      names(res) <- paste0(m[1], c("w", "x", "y", "z"), m[3])
      res
    },
    # TODO: add in other values too.
    {
      warning("unsupported unbundle spec")
      NULL
    }
  )
}

unbundle <- function(df, ..., .keep = "unused") {
  # opposite syntax of bundle
  bv_args <- enquos(...)
  for (i in seq_along(bv_args)) {
    df <- mutate(df, !!!pull_unbundle(names(bv_args)[[i]], bv_args[[i]]), .keep = .keep)
  }
  df
}

# when it comes time for tsting, use this:
# select(starts_with("Head_Rot")) %>%
#   bundle(
#     RootRot = "Head_Rot{ed}", .keep = "all"
#   ) %>%
#   unbundle(
#     "PostRot{ed}" = RootRot
#   ) %>%
#   mutate(
#     rotx_diff = Head_Rotx %% 360 - PostRotx %% 360,
#     roty_diff = Head_Roty %% 360 - PostRoty %% 360,
#     rotz_diff = Head_Rotz %% 360 - PostRotz %% 360
#   ) %>%
#   filter(rotx_diff > 1e-10 | roty_diff > 1e-10 | rotz_diff > 1e-10)

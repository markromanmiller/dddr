
# first person left-right, not third-person left-right. Note that left / right
# is corresponding to a point at the origin, not an observer looking at the
# orgin

# if entry is missing from this table, it's becakse first and second are on the
# same axis.

#' @keywords internal
semantic_directions <- c("up", "down", "left", "right", "forward", "backward")

#' @keywords internal
opposite_direction <- c(
  up = "down", down = "up",
  left = "right", right = "left",
  forward = "backward", backward = "forward"
)

#' @keywords internal
semantic_cross_table <- read.csv(strip.white = T, text = "
     first,   second, lefthand, righthand
        up,     left,  forward,  backward
        up,    right, backward,   forward
        up,  forward,    right,      left
        up, backward,     left,     right
      down,     left, backward,   forward
      down,    right,  forward,  backward
      down,  forward,     left,     right
      down, backward,    right,      left
      left,       up, backward,   forward
      left,     down,  forward,  backward
      left,  forward,       up,      down
      left, backward,     down,        up
     right,       up,  forward,  backward
     right,     down, backward,   forward
     right,  forward,     down,        up
     right, backward,       up,      down
   forward,       up,     left,     right
   forward,     down,    right,      left
   forward,     left,     down,        up
   forward,    right,       up,      down
  backward,       up,    right,      left
  backward,     down,     left,     right
  backward,     left,       up,      down
  backward,    right,     down,        up
")

#' Generate axis and angle semantics
#'
#' Axis and angle semantics are created using `semantics_axes` and
#' `semantics_angle` functions. As there are a lot of conventions, we have both
#' an interface to define your own as well as using ones contributed by the
#' community, such as `semantics_unity`.
#'
#' @param x,y,z String values giving semantic meaning for each positive
#'   direction, relative to the origin. Acceptable values include `up`, `down`,
#'   `left`, `right`, `forward`, and `backward`.
#' @param hand `left` or `right` specifying the handedness of the coordinate
#'   system.
#'
#' @export
semantics_axes <- function(x=NULL, y=NULL, z=NULL, hand=NULL) {

  # test that at least three are present.
  missing_values <- is.null(x) + is.null(y) + is.null(z) + is.null(hand)
  if (missing_values > 1) {
    rlang::abort(
      "At least three arguments should be specified.",
      class = "dddr_semantics"
    )
  }

  # test that xyz are all semantic directions, and that hand is left or right.
  if (!all(c(x, y, z) %in% semantic_directions)) {
    rlang::abort(
      paste(
        "x, y, and z must be specified as one of:",
        paste0(semantic_directions, collapse = ", ")
      ),
      class = "dddr_semantics"
    )
  }
  if (!is.null(hand) && !(hand %in% c("left", "right"))) {
    rlang::abort(
      "`hand`` must be either `left`` or `right`",
      class = "dddr_semantics"
    )
  }

  # find the missing setting based on the other three values
  sct <- semantic_cross_table

  if (is.null(x)) {
    x <- sct[[paste0(hand, "hand")]][
     sct[["first"]] == y &
       sct[["second"]] == z
      ]
  } else if (is.null(y)) {
    y <- sct[[paste0(hand, "hand")]][
      sct[["first"]] == z &
        sct[["second"]] == x
      ]
  } else if (is.null(z)) {
    z <- sct[[paste0(hand, "hand")]][
      sct[["first"]] == x &
        sct[["second"]] == y
      ]
  } else if (is.null(hand)) {
    fs_line <- sct$first == x & sct$second == y
    if (any(fs_line & sct$lefthand == z)) {
      hand <- "left"
    } else if (any(fs_line & sct$righthand == z)) {
      hand <- "right"
    } else {
      hand <- character(0)
    }
  }

  # if one of these is character(0), then it wasn't able to be created,
  # which means there wasn't a valid axes system matching the given criteria.
  if (length(c(x, y, z, hand)) < 4) {
    rlang::abort(
      "Values x, y, and z do not define a valid axes system.",
      class = "dddr_semantics"
    )
  }

  # if all were specified, do a final check that they were consistent.
  if (missing_values == 0) {
    # do a check.
    if (!any(sct[["first"]] == x &
            sct[["second"]] == y &
            sct[[paste0(hand, "hand")]] == z)
        ) {
      rlang::abort(
        "Values x, y, z, and handedness do not define a valid axes system.",
        class = "dddr_semantics"
      )
    }
  }

  # create the output list!
  result <- list("+x", "+y", "+z", "-x", "-y", "-z")
  names(result) <- c(x, y, z, opposite_direction[c(x, y, z)])
  result$hand <- hand
  result
}

set_semantics <- function(axes) {
  options("dddr.convention" = axes)
}

get_semantics <- function(axes) {
  options("dddr.convention")[[1]]
}

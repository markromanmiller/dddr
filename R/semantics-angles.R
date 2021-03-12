#' @export
#'
semantics_angles <- function(intrinsic = NULL, extrinsic = NULL, hand = NULL) {

  if (is.null(hand) || !xor(is.null(intrinsic), is.null(extrinsic))) {
    rlang::abort(paste0(
      "The arguments `hand` and and exactly one of ",
      "`intrinsic` and `extrinsic` must be specified."
    ), class = "dddr_semantics")
  }

  if (!(hand %in% c("left", "right"))) {
    rlang::abort(
      '`hand` must be either "right" or "left".',
      class = "dddr_semantics"
    )
  }

  pry <- c(p = "pitch", r = "roll", y = "yaw")

  if (is.null(intrinsic)) {
    given <- extrinsic
    given_name <- "extrinsic"
    opposite_name <- "intrinsic"
  } else {
    given <- intrinsic
    given_name <- "intrinsic"
    opposite_name <- "extrinsic"
  }

  if (length(given) == 3) {
    if (any(!(given %in% pry))) {
      rlang::abort(paste0(
        "Only \"yaw\", \"pitch\", and \"roll\" and abbreviations are ",
        "accepted values in `semantics_angles`."
      ), class = "dddr_semantics")
    } else {
      clean <- given
    }
  } else if (length(given) == 1 && nchar(given) == 3) {
    clean <- unname(pry[strsplit(given, "")[[1]]])
    if (any(is.na(clean))) {
      rlang::abort(paste0(
        "(Only \"yaw\", \"pitch\", and \"roll\" and abbreviations are ",
        "accepted values in `semantics_angles`."
      ), class = "dddr_semantics")
    }
  } else {
    rlang::abort(paste0(
      "`", given_name, "` must be either a character vector of length 3 ",
      "or a single string with three letters."),
      class = "dddr_semantics"
    )
  }

  result <- list()
  result[[given_name]] <- clean
  result[[opposite_name]] <- rev(clean)
  result$hand <- hand
  result
}

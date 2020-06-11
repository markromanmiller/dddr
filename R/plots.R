#' Spatial Plotting (Coordinates)
#'
#' @name dddr_coords
NULL

#' @importFrom ggplot2 scale_type
#' @method scale_type dddr_vector3
#' @export
scale_type.dddr_vector3 <- function(x) "identity"

#' @export
StatVector3 <- ggplot2::ggproto(
  "StatVector3", ggplot2::Stat,
  compute_layer = function(data, params, layout) {
    data$x <- extract_horizontal_dimension(data$vector3)
    data$y <- extract_vertical_dimension(data$vector3)

    ggplot2::StatIdentity$compute_layer(data, params, layout)
   },
   required_aes = c("vector3")
)

#' @export
stat_vector3 <- function(mapping = NULL, data = NULL, geom = "point",
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatVector3, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# helper here, test if it's a dddr_vector3, and if so, add the view attribute.
tag_views_in_df <- function(df, view) {
  data.frame(lapply(df, function(col) {
    if (inherits(col, "dddr_vector3")) {
      attr(col, "view") <- view
    }
    col
  }))
}


#' @export
CoordLookAtFront <- ggplot2::ggproto(
  "CoordLookAtFront", ggplot2::CoordFixed,
  setup_data = function(self, data, params) {
    # tag the vectors with the kind of view you should expect to give.
    # data is list of dfs.
    data <- lapply(data, tag_views_in_df, view = self$view)

    ggplot2::CoordFixed$setup_data(data, params)
  }
)

#' @rdname dddr_coords
#' @export
coord_look_at <- function(
  direction,
  xlim = NULL, ylim = NULL,
  expand = TRUE, clip = "on"
) {
  # TODO: ensure that direction is one of the reasonable ones.
  ggplot2::ggproto(NULL, CoordLookAtFront,
          limits = list(x = xlim, y = ylim),
          ratio = 1,
          expand = expand,
          clip = clip,
          view = paste("at", direction)
  )
}

#' @rdname dddr_coords
#' @export
coord_look_at_front <- function(...) {coord_look_at("front", ...)}

#' @rdname dddr_coords
#' @export
coord_look_at_back <- function(...) {coord_look_at("back", ...)}

#' @rdname dddr_coords
#' @export
coord_look_at_top <- function(...) {coord_look_at("top", ...)}

#' @rdname dddr_coords
#' @export
coord_look_at_bottom <- function(...) {coord_look_at("bottom", ...)}

#' @rdname dddr_coords
#' @export
coord_look_at_left <- function(...) {coord_look_at("left", ...)}

#' @rdname dddr_coords
#' @export
coord_look_at_right <- function(...) {coord_look_at("right", ...)}

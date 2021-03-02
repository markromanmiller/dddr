#' @importFrom ggplot2 scale_type
#' @method scale_type dddr_vector3
#' @export
scale_type.dddr_vector3 <- function(x) "identity"


#' Stat proto
#'
#' @format NULL
#' @usage NULL
#' @export
StatVector3 <- ggplot2::ggproto(
  "StatVector3", ggplot2::Stat,
  compute_layer = function(self, data, params, layout) {
    data$x <- extract_horizontal(data$vector3)
    data$y <- extract_vertical(data$vector3)

    ggplot2::StatIdentity$compute_layer(data, params, layout)
   },
   required_aes = c("vector3")
)

#' Spatial Plotting (Layers)
#'
#' In order to create layers that use vector3 objects sensibly, they need to be
#' specified within a stat. This ensures clarity (for the reader and for the
#' computer) that the vectors passed in are indeed vectors. Under the hood,
#' these vectors are converted into the right XYZ vectors, so you don't have to
#' think about that conversion yourself.
#'
#' @inheritParams ggplot2::stat_identity
#' @export
stat_vector3 <- function(mapping = NULL, data = NULL, geom = "point",
                        position = "identity", show.legend = NA,
                        inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatVector3, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(...)
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

semantics_reverse <- function(dimension) {
  if (substr(get_semantics()[[dimension]], 1, 1) == "-") {
    return(function(x) (1 - x))
  } else {
    return(function(x) (x))
  }
}

#' Coord proto
#'
#' @format NULL
#' @usage NULL
#' @export
CoordLookAt <- ggplot2::ggproto(
  "CoordLookAt", ggplot2::CoordFixed,
  setup_data = function(self, data, params) {
    # tag the vectors with the kind of view you should expect to give.
    # data is list of dfs.
    data <- lapply(data, tag_views_in_df, view = self$view)
    # also specify whether horz / vert need to be mirrored.

    ggplot2::CoordFixed$setup_data(data, params)
  },

  transform = function(self, data, panel_params) {
    data <- ggplot2::transform_position(
      data, panel_params$x$rescale, panel_params$y$rescale
    )
    data <- ggplot2::transform_position(
      data,
      semantics_reverse(extract_horizontal_dimension(self$view)),
      semantics_reverse(extract_vertical_dimension(self$view))
    )
    ggplot2::transform_position(
      data, scales::squish_infinite, scales::squish_infinite
    )
  }
)

#' Spatial Plotting (Coordinates)
#'
#' To render the view of some spatial object, one must select the way to view
#' it, both by the method (orthogonal, perspective) and the placement of the
#' camera. This is performed using a ggplot2 coord.
#'
#' @inheritParams ggplot2::coord_fixed
#' @param direction String representing the face being looked at.
#' @param ... Values passed along to `coord_look_at`
#'
#' @name dddr_coords
NULL

#' @rdname dddr_coords
#' @export
coord_look_at <- function(
  direction,
  xlim = NULL, ylim = NULL,
  expand = TRUE, clip = "on"
) {
  # TODO: ensure that direction is one of the reasonable ones.
  ggplot2::ggproto(NULL, CoordLookAt,
          limits = list(x = xlim, y = ylim),
          ratio = 1,
          expand = expand,
          clip = clip,
          view = paste("at", direction)
  )
}

#' @rdname dddr_coords
#' @export
coord_look_at_front <- function(...) {
  coord_look_at("front", ...)
}

#' @rdname dddr_coords
#' @export
coord_look_at_back <- function(...) {
  coord_look_at("back", ...)
}

#' @rdname dddr_coords
#' @export
coord_look_at_top <- function(...) {
  coord_look_at("top", ...)
}

#' @rdname dddr_coords
#' @export
coord_look_at_bottom <- function(...) {
  coord_look_at("bottom", ...)
}

#' @rdname dddr_coords
#' @export
coord_look_at_left <- function(...) {
  coord_look_at("left", ...)
}

#' @rdname dddr_coords
#' @export
coord_look_at_right <- function(...) {
  coord_look_at("right", ...)
}

# Some objects are simple to convert to 3D. These are not those objects.

#' @rdname dddr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatBin2d3 <- ggplot2::ggproto(
  "StatBin2d3", ggplot2::StatBin2d,
  compute_layer = function(self, data, params, layout) {

    # if aesthetic is not present, just silently pass;
    # it'll be checked (and error thrown) by the parent class's compute_layer
    if ("v" %in% names(data)) {
      data <- extract_vector3(data)
      x_proto <- ggplot2::scale_x_continuous()
      y_proto <- ggplot2::scale_y_continuous()
      layout$train_position(list(data), x_proto, y_proto)
    }

    parent <- ggplot2::ggproto_parent(ggplot2::StatBin2d, self)

    parent$compute_layer(data, params, layout)
  },
  required_aes = c("v"),
  dropped_aes = c("v", "depth"),
)

#' @rdname geom_bin2d3
#' @inheritParams ggplot2::stat_bin_2d
#' @export
stat_bin_2d3 <- function(mapping = NULL, data = NULL,
                         geom = "tile", position = "identity",
                         ...,
                         bins = 30,
                         binwidth = NULL,
                         drop = TRUE,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatBin2d3,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      bins = bins,
      binwidth = binwidth,
      drop = drop,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_bin2d3
#' @export
stat_bin2d3 <- stat_bin_2d3

#' Spatial Plotting (Binned Data)
#'
#' To bin data points according to a view, the points need to be converted from
#' vectors into 2D points. These geoms and stats perform this transformation.
#' Only `v` is a required aesthetics.
#'
#' @inheritParams ggplot2::geom_bin2d
#' @export
geom_bin2d3 <- function(mapping = NULL, data = NULL,
                         stat = "bin2d3", position = "identity",
                         ...,
                         bins = 30,
                         binwidth = NULL,
                         drop = TRUE,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatBin2d3,
    geom = ggplot2::GeomTile,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      bins = bins,
      binwidth = binwidth,
      drop = drop,
      na.rm = na.rm,
      ...
    )
  )
}

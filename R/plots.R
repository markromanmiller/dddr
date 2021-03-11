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
   compute_group = function(self, data, scales) {
     extract_vector3(data)
   },
   required_aes = c("vector3")
)

Stat3Bin2d <- ggplot2::ggproto(
  "Stat3Bin2d", ggplot2::StatBin2d,
  compute_layer = function(self, data, params, layout) {

    # if aesthetic is not present, just silently pass;
    # it'll be checked (and error thrown) by the parent class's compute_layer
    if ("vector3" %in% names(data)) {
      data <- extract_vector3(data)
      x_proto <- ggplot2::scale_x_continuous()
      y_proto <- ggplot2::scale_y_continuous()
      layout$train_position(list(data), x_proto, y_proto)
    }

    parent <- ggplot2::ggproto_parent(ggplot2::StatBin2d, self)

    parent$compute_layer(data, params, layout)
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

stat3_bin_2d <- function(mapping = NULL, data = NULL,
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
    stat = Stat3Bin2d,
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

# look_at_left vs. looking_left (())

# xy_plane xz_plane, etc. (follow handedness)

# show the axes in the plot - > use "annotation_custom" with a custom grob


# have some custom code here that defines a nice easy plot.

# have it here? idk.

#' Spatial Plotting
#'
#'
#' TODO: discur
#'
#' @importFrom ggplot2 ggproto
#'
NULL

#' @export
scale_type.dddr_vector3 <- function(x) "identity"

StatPoint3 <- ggplot2::ggproto(
  "StatPoint3", ggplot2::Stat,
  compute_layer = function(data, params, layout) {
    #print("stat compute_layer")
    #print(attributes(data$vector3))
    data$x <- extract_horizontal_dimension(data$vector3)
    data$y <- extract_vertical_dimension(data$vector3)

    StatIdentity$compute_layer(data, params, layout)
   },
   required_aes = c("vector3")
)

stat_point3 <- function(mapping = NULL, data = NULL, geom = "point",
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatPoint3, data = data, mapping = mapping, geom = geom,
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


# what if we include a little change to Vector3 such that x ad y are only pulled sensibly
CoordLookAtFront <- ggplot2::ggproto(
  "CoordLookAtFront", ggplot2::CoordFixed,
  setup_data = function(data, params) {
    # tag the vectors with the kind of view you should expect to give.
    data <- lapply(data, tag_views_in_df, view="AtFront") # data is list of dfs.

    CoordFixed$setup_data(data, params)
  }
)


coord_look_at_front <- function(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") {
  ggplot2::ggproto(NULL, CoordLookAtFront,
          limits = list(x = xlim, y = ylim),
          ratio = 1,
          expand = expand,
          clip = clip
  )
}

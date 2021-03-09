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
  },

  render_fg = function(self, panel_params, theme) {

    # create the inherited objects:
    coord_fixed_fg <- ggplot2::CoordFixed$render_fg(panel_params, theme)
    if (!(theme$dddr.rose.location %in% c("tl", "tr", "bl", "br"))) {
      return(coord_fixed_fg)
    }

    # calculate the viewport we're working with.
    rose_margin <- theme$dddr.rose.margin
    rose_length <- theme$dddr.rose.length

    if (grepl("r", theme$dddr.rose.location)) {
      origin_x <- grid::unit(1, units="npc") - rose_margin[2] - rose_length
    } else {
      origin_x <- grid::unit(0, units="npc") + rose_margin[4] + rose_length
    }
    if (grepl("t", theme$dddr.rose.location)) {
      origin_y <- grid::unit(1, units="npc") - rose_margin[1] - rose_length
    } else {
      origin_y <- grid::unit(0, units="npc") + rose_margin[3] + rose_length
    }
    rose_viewport <- grid::viewport(
      x = origin_x, y = origin_y,
      width = 2*rose_length, height = 2*rose_length
    )

    # get all the directions for use in placing texts:
    horz_dim <- get_semantics()[[extract_horizontal_dimension(self$view)]]
    horz_axis <- get_axis(horz_dim)
    horz_direction <- get_direction(horz_dim)
    axis_hjust <- switch(horz_direction, "+" = 1.6, "-" = -0.6)
    horz_placement <- switch(horz_direction, "+" = 1, "-" = 0)

    vert_dim <- get_semantics()[[extract_vertical_dimension(self$view)]]
    vert_axis <- get_axis(vert_dim)
    vert_direction <- get_direction(vert_dim)
    axis_vjust <- switch(vert_direction, "+" = 1.6, "-" = -0.6)
    vert_placement <- switch(vert_direction, "+" = 1, "-" = 0)

    norm_dim <- get_semantics()[[extract_normal_dimension(self$view)]]
    norm_axis <- get_axis(norm_dim)
    norm_direction <- get_direction(norm_dim)

    # produce horizontal objects
    horz_line <- ggplot2::element_render(
      theme, "dddr.rose.line",
      x = c(0.5, horz_placement),
      y = c(0.5, 0.5),
      colour = theme[[paste0("dddr.rose.color.", horz_axis)]]
    )
    horz_text <- ggplot2::element_render(
      theme, "dddr.rose.text.horz",
      label = toupper(horz_axis),
      x = horz_placement, y = 0.5,
      hjust = horz_placement, vjust = axis_vjust,
      colour = theme[[paste0("dddr.rose.color.", horz_axis)]]
    )

    # produce vertical objects
    vert_line <- ggplot2::element_render(
      theme, "dddr.rose.line",
      x = c(0.5, 0.5),
      y = c(0.5, vert_placement),
      colour = theme[[paste0("dddr.rose.color.", vert_axis)]]
    )
    vert_text <- ggplot2::element_render(
      theme, "dddr.rose.text.vert",
      label = toupper(vert_axis),
      x = 0.5, y = vert_placement,
      hjust = axis_hjust, vjust = vert_placement,
      colour = theme[[paste0("dddr.rose.color.", vert_axis)]]
    )

    # produce normal objects
    norm_point <- ggplot2::element_render(
      theme,
      "dddr.rose.point",
      label = switch(
        norm_direction,
        "+" = theme[["dddr.rose.point.towards"]],
        "-" = theme[["dddr.rose.point.away"]]
      ),
      x = 0.5, y = 0.5,
      hjust = 0.5, vjust = 0.5,
      colour = theme[[paste0("dddr.rose.color.", norm_axis)]]
    )
    norm_text <- ggplot2::element_render(
      theme, "dddr.rose.text.norm",
      label = toupper(norm_axis),
      x = 0.5, y = 0.5,
      hjust = axis_hjust, vjust = axis_vjust,
      colour = theme[[paste0("dddr.rose.color.", norm_axis)]]
    )

    # return the grobs to render
    grid::gList(
      coord_fixed_fg,
      grid::grobTree(
        horz_line, vert_line, norm_point,
        horz_text, vert_text, norm_text,
        vp = rose_viewport
      )
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

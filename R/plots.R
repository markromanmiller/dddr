# look_at_left vs. looking_left (())

# xy_plane xz_plane, etc. (follow handedness)

# show the axes in the plot - > use "annotation_custom" with a custom grob


# have some custom code here that defines a nice easy plot.

# have it here? idk.

#' @export
scale_type.dddr_vector3 <- function(x) "identity"

StatPoint3 <- ggproto(
  "StatPoint3", Stat,
  compute_layer = function(data, params, layout) {
    data$x <- data$vector3$x
    data$y <- data$vector3$y
    data$z <- data$vector3$z
    #print(params)
    print("stat compute later")
    #print(layout)
    #print(layout$DDDR_VIEW)
    #data$vector3 <- NULL
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

flip_axis_labels <- function(x) {
  old_names <- names(x)

  new_names <- old_names
  new_names <- gsub("^x", "tmp", new_names)
  new_names <- gsub("^y", "x", new_names)
  new_names <- gsub("^z", "y", new_names)
  new_names <- gsub("^tmp", "z", new_names)

  setNames(x, new_names)
}


CoordLookAtFront <- ggproto(
  "CoordLookAtFront", CoordFixed,

  transform = function(data, panel_params) {
    print("FOOBAR")
    data <- flip_axis_labels(data)
    CoordFixed$transform(data, panel_params)
  },
  backtransform_range = function(self, panel_params) {
    self$range(panel_params)
  },
  range = function(self, panel_params) { # ...?
    # summarise_layout() expects the original x and y ranges here,
    # not the ones we would get after flipping the axes
    un_flipped_range <- ggproto_parent(CoordCartesian, self)$range(panel_params)
    print(un_flipped_range$x)
    list(x = un_flipped_range$y, y = un_flipped_range$x)
  },
  setup_panel_params = function(self, scale_x, scale_y, params = list()) {
    parent <- ggproto_parent(CoordCartesian, self)
    panel_params <- parent$setup_panel_params(scale_x, scale_y, params)
    flip_axis_labels(panel_params)
  },
  labels = function(labels, panel_params) {
    flip_axis_labels(CoordCartesian$labels(labels, panel_params))
  },

  setup_layout = function(layout, params) {
    # Switch the scales
    print(layout)
    #layout[c("SCALE_X", "SCALE_Y", "SCALE_Z")] <- layout[c("SCALE_Y", "SCALE_Z", "SCALE_X")]
    # can I add something to layout?

  },

  modify_scales = function(scales_x, scales_y) {
    #lapply(scales_x, scale_flip_axis)
    #lapply(scales_y, scale_flip_axis)
  }
)

# In-place modification of a scale position to swap axes
scale_flip_axis <- function(scale) {
  print(scale)
  scale$position <- switch(scale$position,
                           top = "right",
                           bottom = "left",
                           left = "bottom",
                           right = "top",
                           scale$position
  )

  invisible(scale)
}



# I don't remember how this one split out...
CoordLookAtFront <- ggproto(
  "CoordLookAtFront", CoordFixed,
  setup_data = function(data, params) {
    print("setup_data")
    print(data)
    print(params)
    CoordFixed$setup_data(data, params)
  },
  transform = function(self, data, panel_params) {
    print("transform")
    if ("guides" %in% names(panel_params)) {
      print("transform skipped")
    } else {
      print(data %>% head(3))
      self$transformed_data <- data
      print(names(panel_params))
      data$x <- data$x + sin(data$z)
      #panel_
      tmp <- data$x
      data$x <- data$y
      data$y <- data$z
      data$z <- tmp

      print(panel_params$y)
      print(panel_params$y.range)
      #panel_params$y$train(data$z)

      #print(panel_params$y)
    }
    # this is called a bunch of different times: once for each layer, and once for

    CoordFixed$transform(data, panel_params)
  }
)

# what if we include a little change to Vector3 such that x ad y are only pulled sensibly
CoordLookAtFront <- ggproto(
  "CoordLookAtFront", CoordFixed,
  setup_data = function(data, params) {

    # lapply foo

    print("setup_data")
    #print(data)
    #print(params)
    CoordFixed$setup_data(data, params)
  },
  transform = function(self, data, panel_params) {
    print("transform")
    if ("guides" %in% names(panel_params)) {
      print("transform skipped")
    } else {
      #print(data %>% head(3))
      self$transformed_data <- data
      #print(names(panel_params))
      data$x <- data$x + sin(data$z)
      #panel_
      tmp <- data$x
      data$x <- data$y
      data$y <- data$z
      data$z <- tmp

      #print(panel_params$y)
      #print(panel_params$y.range)
      #panel_params$y$train(data$z)

      #print(panel_params$y)
      #
    }

    # this is called a bunch of different times: once for each layer, and once for
    CoordFixed$transform(data, panel_params)
  },
  setup_layout = function(layout, params) {
    # Switch the scales
    #print(layout)
    #layout[c("SCALE_X", "SCALE_Y", "SCALE_Z")] <- layout[c("SCALE_Y", "SCALE_Z", "SCALE_X")]
    # can I add something to layout?
    layout$DDDR_VIEW <- "AtFront"
    #print(layout)
    layout
  },
  setup_panel_params = function(self, scale_x, scale_y, params = list()) {
    print("setup_panel_params")
    parent <- ggproto_parent(CoordCartesian, self)
    panel_params <- parent$setup_panel_params(scale_x, scale_y, params)
    panel_params
  }
)


coord_look_at_front <- function(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") {
  ggproto(NULL, CoordLookAtFront,
          limits = list(x = xlim, y = ylim),
          ratio = 1,
          expand = expand,
          clip = clip
  )
}

simple_dddr_plot <- function() {
  spiral <- tibble(i = seq(0, 10*pi, 0.05)) %>%
    mutate(
      circular_part = vector3(x=cos(i), y=sin(i), z=i/15),
      forward_part = vector3(x=0, y=0, z=i/15),
      spiral_part = circular_part * i / 30 + forward_part
    )

  spiral %>%
    ggplot() +
    stat_point3(aes(vector3=spiral_part)) +
    coord_look_at_front()
}

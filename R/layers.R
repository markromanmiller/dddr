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
   required_aes = c("v")
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



#' @export
geom_point3 <- function(mapping = NULL, data = NULL,
                        stat = "vector3", position = "identity",
                        ...,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = ggplot2::GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @export
geom_path3 <- function(mapping = NULL, data = NULL,
                       stat = "vector3", position = "identity",
                       ...,
                       lineend = "butt",
                       linejoin = "round",
                       linemitre = 10,
                       arrow = NULL,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = ggplot2::GeomPath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}


#' @export
StatSegment3 <- ggplot2::ggproto(
  "StatSegment3", ggplot2::Stat,
  compute_group = function(self, data, scales) {
    data <- extract_vector3(data)
    data$xend <- extract_horizontal(data$vend)
    data$yend <- extract_vertical(data$vend)
    data$depthend <- extract_normal(data$vend)
    data
  },
  required_aes = c("v", "vend")
)

#' @export
stat_segment3 <- function(mapping = NULL, data = NULL,
                          geom = "segment", position = "identity",
                          ...,
                          arrow = NULL,
                          arrow.fill = NULL,
                          lineend = "butt",
                          linejoin = "round",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatSegment3,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = arrow,
      arrow.fill = arrow.fill,
      lineend = lineend,
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
}

#' @export
geom_segment3 <- function(mapping = NULL, data = NULL,
                          stat = "segment3", position = "identity",
                          ...,
                          arrow = NULL,
                          arrow.fill = NULL,
                          lineend = "butt",
                          linejoin = "round",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = ggplot2::GeomSegment,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = arrow,
      arrow.fill = arrow.fill,
      lineend = lineend,
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
}


#' @export
StatSpoke3 <- ggplot2::ggproto(
  "StatSpoke3", ggplot2::Stat,
  compute_layer = function(self, data, scales, ...) {
    # this is done in layer not group in order to type-check arguments.
    if (!inherits(data$v, "dddr_vector3")) {
      rlang::abort("In geom_spoke3 and stat_spoke3, `v` aesthetic must be a vector3.", class="dddr_plotting")
    } else if (!inherits(data$rot, "dddr_quat")) {
      rlang::abort("In geom_spoke3 and stat_spoke3, `rot` aesthetic must be a quat.", class="dddr_plotting")
    } else if (!inherits(data$radius, "dddr_vector3")) {
      rlang::abort("In geom_spoke3 and stat_spoke3, `radius` aesthetic must be a vector3.", class="dddr_plotting")
    }

    # radius, if specified outside a df, doesn't have the tag for what view the coord is wanting...
    # so borrow the tag from data$v

    # pull out the right values.
    data <- extract_vector3(data)
    data$vend <- data$v + rotate(data$radius, data$rot)
    attr(data$vend, "view") <- attr(data$v, "view")
    data$xend <- extract_horizontal(data$vend)
    data$yend <- extract_vertical(data$vend)
    data$depthend <- extract_normal(data$vend)
    data
  },
  required_aes = c("v", "rot", "radius")#,
  #default_aes = c(radius = ) # I want the 'forward' vector.
)


#' @export
stat_spoke3 <- function(mapping = NULL, data = NULL,
                        geom = "segment", position = "identity",
                        ...,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    geom = geom,
    stat = StatSpoke3,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

geom_spoke3 <- function(mapping = NULL, data = NULL,
                       stat = "spoke3", position = "identity",
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    geom = ggplot2::GeomSegment,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}








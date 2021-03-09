# nocov start

# Load the options for dddr
dddr_set_options <- function() {
  op <- options()
  op.dddr <- list(
    dddr.convention = NULL
  )
  toset <- !(names(op.dddr) %in% names(op))
  if (any(toset)) options(op.dddr[toset])
}

# Load the suggested dependencies for s3 methods
dddr_register_s3_deps <- function() {
  vctrs::s3_register("waldo::compare_proxy", "dddr_quat")
}

# Register the custom dddr elements in ggplot's theme tree
dddr_register_ggplot_theme_elements <- function() {
  ggplot2::register_theme_elements(
    # specify all the new entries a theme can have,
    dddr.rose.line = ggplot2::element_line(),
    dddr.rose.length = grid::unit(30, units="pt"),
    dddr.rose.point = ggplot2::element_text(),
    dddr.rose.point.towards = "\u25CB",
    dddr.rose.point.away = "\u2715",
    dddr.rose.color = as.character(NA),
    dddr.rose.color.x = "#ff0000",
    dddr.rose.color.y = "#00cc00",
    dddr.rose.color.z = "#0000ff",
    dddr.rose.text = ggplot2::element_text(),
    dddr.rose.text.horz = ggplot2::element_text(),
    dddr.rose.text.vert = ggplot2::element_text(),
    dddr.rose.text.norm = ggplot2::element_text(),
    dddr.rose.location = "tr",
    dddr.rose.margin = ggplot2::margin(24, 24, 24, 24, unit="pt"),
    # and specify the inheritance and type structures
    element_tree = list(
      dddr.rose.line = ggplot2::el_def("element_line", "line"),
      dddr.rose.length = ggplot2::el_def("unit"),
      dddr.rose.point = ggplot2::el_def("element_text", "text"),
      dddr.rose.point.towards = ggplot2::el_def("character", NULL),
      dddr.rose.point.away = ggplot2::el_def("character", NULL),
      dddr.rose.color = ggplot2::el_def("character", NULL),
      dddr.rose.color.x = ggplot2::el_def("character", "dddr.rose.color"),
      dddr.rose.color.y = ggplot2::el_def("character", "dddr.rose.color"),
      dddr.rose.color.z = ggplot2::el_def("character", "dddr.rose.color"),
      dddr.rose.text = ggplot2::el_def("element_text", "text"),
      dddr.rose.text.horz = ggplot2::el_def("element_text", "dddr.rose.text"),
      dddr.rose.text.vert = ggplot2::el_def("element_text", "dddr.rose.text"),
      dddr.rose.text.norm = ggplot2::el_def("element_text", "dddr.rose.text"),
      dddr.rose.location = ggplot2::el_def("character", NULL),
      dddr.rose.margin = ggplot2::el_def("margin", "margin")
    )
  )
}

.onLoad <- function(libname, pkgname) {

  dddr_set_options()
  dddr_register_s3_deps()
  dddr_register_ggplot_theme_elements()

  invisible()
}
# nocov end

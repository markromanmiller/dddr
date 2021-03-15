spiral <- data.frame(i = seq(0, 10 * pi, 0.05)) %>%
  dplyr::mutate(
    # vector3s are created using three numeric vector arguments,
    # and thanks to dplyr, can refer to other columns in the dataframe
    circular_part = vector3(x = cos(i), y = sin(i), z = 0),
    forward_part = vector3(x = 0, y = 0, z = i / 15),
    # vector3s can be added together and multiplied by numerics
    spiral_part = circular_part * i / 30 + forward_part,
    sample_quat = axis_angle(axis = circular_part, angle = i)
  )

test_that("Simple example doppleganger works.", {

  set_dddr_semantics(
    axes = semantics_axes(y = "up", z = "forward", hand = "right")
  )

  points_front <- spiral %>%
    ggplot2::ggplot(ggplot2::aes(v = spiral_part)) +
    stat_vector3(geom = "point") +
    coord_look_at_front()

  vdiffr::expect_doppelganger(
    "Testing example",
    points_front,
    path = "plots"
  )
})


test_that("Negative scales are drawn correctly and geom_point3 works", {

  set_dddr_semantics(
    axes = semantics_axes(y = "up", z = "forward", hand = "left")
  )

  points_front <- spiral %>%
    ggplot2::ggplot(ggplot2::aes(v = spiral_part)) +
    geom_point3() +
    coord_look_at_front() +
    ggplot2::theme_test() +
    ggplot2::theme(dddr.rose.location = "none")

  vdiffr::expect_doppelganger(
    "negative scales",
    points_front,
    path = "plots"
  )
})

test_that("Looking back, arrow into screen, and top_left", {

  set_dddr_semantics(
    axes = semantics_axes(y = "up", z = "forward", hand = "right")
  )

  points_front <- spiral %>%
    ggplot2::ggplot(ggplot2::aes(v = spiral_part)) +
    stat_vector3(geom = "point") +
    coord_look_at_back() +
    ggplot2::theme_test() +
    ggplot2::theme(dddr.rose.location = "bl")

  vdiffr::expect_doppelganger(
    "arrow into screen",
    points_front,
    path = "plots"
  )
})

test_that("Looking at the top works", {

  set_dddr_semantics(
    axes = semantics_axes(y = "up", z = "forward", hand = "right")
  )

  points_top <- spiral %>%
    ggplot2::ggplot(ggplot2::aes(v = spiral_part)) +
    stat_vector3(geom = "point") +
    coord_look_at_top()

  vdiffr::expect_doppelganger(
    "spiral from the top",
    points_top,
    path = "plots"
  )
})

test_that("Stat works for both point and line", {

  set_dddr_semantics(
    axes = semantics_axes(y = "up", z = "forward", hand = "right")
  )

  point_and_line <- spiral %>%
    ggplot2::ggplot(ggplot2::aes(v = spiral_part)) +
    geom_point3() +
    geom_path3() +
    coord_look_at_top()

  vdiffr::expect_doppelganger(
    "point_and_line_stat",
    point_and_line,
    path = "plots"
  )
})

test_that("Geom works for both point and line", {

  set_dddr_semantics(
    axes = semantics_axes(y = "up", z = "forward", hand = "right")
  )

  point_and_line <- spiral %>%
    ggplot2::ggplot(ggplot2::aes(v = spiral_part)) +
    stat_vector3(geom = "point") +
    stat_vector3(geom = "path") +
    coord_look_at_top()

  vdiffr::expect_doppelganger(
    "point_and_line_geom",
    point_and_line,
    path = "plots"
  )
})

test_that("segment3 works", {
  set_dddr_semantics(
    axes = semantics_axes(y = "up", z = "forward", hand = "right")
  )

  segment_plot <- spiral %>%
    # only take every nth row (overplotting)
    subset(1:nrow(spiral) %% 10 == 0) %>%
    ggplot2::ggplot(ggplot2::aes(v = spiral_part, vend = forward_part)) +
    geom_segment3(ggplot2::aes(vend = circular_part), color = "red") +
    stat_segment3(color = "blue") +
    coord_look_at_top()

  vdiffr::expect_doppelganger(
    "segment plot",
    segment_plot,
    path = "plots"
  )
})

test_that("spoke3 works", {
  set_dddr_semantics(
    axes = semantics_axes(y = "up", z = "forward", hand = "right"),
    angles = semantics_angles_unity
  )

  spoke_plot <- spiral %>%
    ggplot2::ggplot(ggplot2::aes(v = spiral_part, rot = sample_quat)) +
    geom_spoke3(ggplot2::aes(radius = vector3(0.1, 0, 0)), color = "red") +
    stat_spoke3(ggplot2::aes(radius = vector3(0, 0, 0.1)), color = "blue") +
    coord_look_at_top()

  vdiffr::expect_doppelganger(
    "spoke plot",
    spoke_plot,
    path = "plots"
  )
})

test_that("Stat3 bin2d works", {
  set_dddr_semantics(
    axes = semantics_axes(y = "up", z = "forward", hand = "right")
  )

  stat3_bin2d <- spiral %>%
    ggplot2::ggplot(ggplot2::aes(v = spiral_part)) +
    stat_bin_2d3() +
    coord_look_at_top()

  vdiffr::expect_doppelganger(
    "stat3_bin2d",
    stat3_bin2d,
    path = "plots"
  )
})


test_that("Plots with missing or wrong aesthetics give an error.", {

  expected_error <- "requires the following missing aesthetics: v"

  set_dddr_semantics(
    axes = semantics_axes(y = "up", z = "forward", hand = "right")
  )

  aes_error_plot <- spiral %>%
    ggplot2::ggplot(ggplot2::aes(foobar = spiral_part)) +
    stat_vector3(geom = "point") +
    coord_look_at_front()

  expect_error(ggplot2::ggplot_build(aes_error_plot), expected_error)

})

test_that("a plot without semantics should error", {

  expected_error <- "axis semantics are null"

  set_dddr_semantics(axes = NULL)

  axis_semantics <- spiral %>%
    ggplot2::ggplot(ggplot2::aes(v = spiral_part)) +
    stat_vector3(geom = "point") +
    coord_look_at_front()

  expect_error(ggplot2::ggplot_build(axis_semantics), expected_error)

})

test_that("after_stat depth works", {

  set_dddr_semantics(
    axes = semantics_axes(y = "up", z = "forward", hand = "right")
  )

  after_stat_depth <- spiral %>%
    ggplot2::ggplot(
      ggplot2::aes(
        v = spiral_part,
        color = ggplot2::after_stat(depth)
      )
    ) +
    stat_vector3(geom = "point") +
    coord_look_at_top()

  vdiffr::expect_doppelganger(
    "after_stat_depth",
    after_stat_depth,
    path = "plots"
  )
})

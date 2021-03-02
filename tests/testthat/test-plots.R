spiral <- data.frame(i = seq(0, 10 * pi, 0.05)) %>%
  dplyr::mutate(
    # vector3s are created using three numeric vector arguments,
    # and thanks to dplyr, can refer to other columns in the dataframe
    circular_part = vector3(x = cos(i), y = sin(i), z = 0),
    forward_part = vector3(x = 0, y = 0, z = i/15),
    # vector3s can be added together and multiplied by numerics
    spiral_part = circular_part * i / 30 + forward_part
  )

test_that("Simple example doppleganger works.", {

  set_semantics(semantics_axes(y = "up", z = "forward", hand = "right"))

  points_front <- spiral %>%
    ggplot2::ggplot(ggplot2::aes(vector3 = spiral_part)) +
    stat_vector3(geom = "point") +
    coord_look_at_front()

  vdiffr::expect_doppelganger(
    "Testing example",
    points_front
  )
})


test_that("Negative scales are drawn correctly.", {

  set_semantics(semantics_axes(y = "up", z = "forward", hand = "left"))

  points_front <- spiral %>%
    ggplot2::ggplot(ggplot2::aes(vector3 = spiral_part)) +
    stat_vector3(geom = "point") +
    coord_look_at_front()

  vdiffr::expect_doppelganger(
    "negative scales",
    points_front
  )
})

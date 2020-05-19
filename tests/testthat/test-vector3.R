context("vector3 basics")
library(dplyr)

sqrt_1_3 <- sqrt(1/3)

foo_px <- c(1, 0, 0, sqrt_1_3)
foo_py <- c(0, 1, 0, sqrt_1_3)
foo_pz <- c(0, 0, 1, sqrt_1_3)

simple_axes_tbl <- data.frame(foo = vector3(x=foo_px, y=foo_py, z=foo_pz))

test_that("Vector3 entries can be extracted", {
  extracted <- dplyr::mutate(simple_axes_tbl,
      foo_px = foo$x,
      foo_py = foo$y,
      foo_pz = foo$z
    )
  expect_equal(extracted$foo_px, foo_px)
  expect_equal(extracted$foo_py, foo_py)
  expect_equal(extracted$foo_pz, foo_pz)
})

test_that("Vector3 entries can be added", {
  doubled <- dplyr::mutate(simple_axes_tbl,
      bar = foo + foo,
      bar_px = bar$x,
      bar_py = bar$y,
      bar_pz = bar$z
    )

  bar_px <- c(2, 0, 0, 2*sqrt_1_3)
  bar_py <- c(0, 2, 0, 2*sqrt_1_3)
  bar_pz <- c(0, 0, 2, 2*sqrt_1_3)

  expect_equal(doubled$bar_px, bar_px)
  expect_equal(doubled$bar_py, bar_py)
  expect_equal(doubled$bar_pz, bar_pz)
})

test_that("Vector3 entries can be added to numeric vectors of length 3", {
  translated_z <- dplyr::mutate(simple_axes_tbl,
      bar = foo + c(0, 0, 1),
      bar_px = bar$x,
      bar_py = bar$y,
      bar_pz = bar$z
    )
  expect_equal(translated_z$bar_px, foo_px)
  expect_equal(translated_z$bar_py, foo_py)
  expect_equal(translated_z$bar_pz, c(1, 1, 2, 1+sqrt_1_3))

  translated_x <- dplyr::mutate(simple_axes_tbl,
      bar = c(1, 0, 0) + foo,
      bar_px = bar$x,
      bar_py = bar$y,
      bar_pz = bar$z
    )
  expect_equal(translated_x$bar_px, c(2, 1, 1, 1+sqrt_1_3))
  expect_equal(translated_x$bar_py, foo_py)
  expect_equal(translated_x$bar_pz, foo_pz)

})

test_that("Vector3 entries can't be added to numeric vectors of various non-3 sizes", {
  error_class = "dplyr_error"
  message = "the numeric adding to a vector3 must have length 3"

  expect_error(dplyr::mutate(simple_axes_tbl, bar = foo + c(0, 0, 1, 0)), class = error_class, regexp = message)
  expect_error(dplyr::mutate(simple_axes_tbl, bar = c(0)+ foo), class = error_class, regexp = message)
  expect_error(dplyr::mutate(simple_axes_tbl, bar = foo + c(0, 0)), class = error_class, regexp = message)
  expect_error(dplyr::mutate(simple_axes_tbl, bar = rep(1, 15) + foo), class = error_class, regexp = message)
})

test_that("Vector3 can be multiplied both left and right by numeric of length 1", {
  doubled <- dplyr::mutate(simple_axes_tbl,
      bar = foo * 2,
      baz = 3 * foo
    )

  expect_equal(doubled$bar$x, c(2, 0, 0, 2*sqrt_1_3))
  expect_equal(doubled$bar$y, c(0, 2, 0, 2*sqrt_1_3))
  expect_equal(doubled$bar$z, c(0, 0, 2, 2*sqrt_1_3))

  expect_equal(doubled$baz$x, c(3, 0, 0, 3*sqrt_1_3))
  expect_equal(doubled$baz$y, c(0, 3, 0, 3*sqrt_1_3))
  expect_equal(doubled$baz$z, c(0, 0, 3, 3*sqrt_1_3))

})

test_that("Vector3 can be multiplied both left and right by numeric vector the same length as the vector3 vector", {
  doubled <- dplyr::mutate(simple_axes_tbl,
      fac = seq(1, 7, by=2),
      bar = foo * c(1, 2, 3, 4),
      baz = fac * foo
    )

  expect_equal(doubled$bar$x, c(1, 0, 0, 4*sqrt_1_3))
  expect_equal(doubled$bar$y, c(0, 2, 0, 4*sqrt_1_3))
  expect_equal(doubled$bar$z, c(0, 0, 3, 4*sqrt_1_3))

  expect_equal(doubled$baz$x, c(1, 0, 0, 7*sqrt_1_3))
  expect_equal(doubled$baz$y, c(0, 3, 0, 7*sqrt_1_3))
  expect_equal(doubled$baz$z, c(0, 0, 5, 7*sqrt_1_3))

})

test_that("Vector3 distances are correctly computed", {
  distances <- dplyr::mutate(data.frame(
    foo = vector3(
      x = c(-.5, .5, sqrt(2)/2, 3),
      y = c(-.5, -sqrt(2)/2, .5, 4),
      z = c(sqrt(2)/2, .5, -.5, 0)
    )),
    distance = distance(foo)
  )

  expect_equal(distances$distance, c(1, 1, 1, 5))
})

test_that("Vector3 distances are correctly computed from an offset", {
  distances <- dplyr::mutate(data.frame(
    foo = vector3(
      x = c( 1, -4,  4, 3),
      y = c( 0, -3,  5, 4),
      z = c(-2,  5, -7, 0)
    )),
      distance = distance(foo, from=c(0, 2, -3))
    )

  expect_equal(distances$distance, c(sqrt(6), sqrt(105), sqrt(41), sqrt(22)))
})

test_that("Vector3 can be normalized", {
  normalized <- dplyr::mutate(data.frame(
    foo = vector3(
      x = c( 1, 0,  4, 3),
      y = c( 0, 0,  5, 4),
      z = c(-1, 5, -7, 0)
    )),
      unit = normalize(foo),
      normalized_distance = normalize(foo) %>% distance,
      normalized_distance_of_5 = normalize(foo, length=5) %>% distance
    )

  expect_equal(head(normalized$unit$x, 2), c(sqrt(2)/2, 0))
  expect_equal(head(normalized$unit$y, 2), c(0, 0))
  expect_equal(head(normalized$unit$z, 2), c(-sqrt(2)/2, 1))

  expect_equal(normalized$normalized_distance, c(1, 1, 1, 1))
  expect_equal(normalized$normalized_distance_of_5, c(5, 5, 5, 5))
})


















context("vector3 basics")
library(vrmvrm)
library(dplyr)

sqrt_1_3 <- sqrt(1/3)

foo_px <- c(1, 0, 0, sqrt_1_3)
foo_py <- c(0, 1, 0, sqrt_1_3)
foo_pz <- c(0, 0, 1, sqrt_1_3)

simple_axes_tbl <- data.frame(foo = vector3(x=foo_px, y=foo_py, z=foo_pz))

test_that("Vector3 entries can be extracted", {
  extracted <- simple_axes_tbl %>%
    mutate(
      foo_px = foo$x,
      foo_py = foo$y,
      foo_pz = foo$z
    )
  expect_equal(extracted$foo_px, foo_px)
  expect_equal(extracted$foo_py, foo_py)
  expect_equal(extracted$foo_pz, foo_pz)
})

test_that("Vector3 entries can be added", {
  doubled <- simple_axes_tbl %>%
    mutate(
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
  translated_z <- simple_axes_tbl %>%
    mutate(
      bar = foo + c(0, 0, 1),
      bar_px = bar$x,
      bar_py = bar$y,
      bar_pz = bar$z
    )
  expect_equal(translated_z$bar_px, foo_px)
  expect_equal(translated_z$bar_py, foo_py)
  expect_equal(translated_z$bar_pz, c(1, 1, 2, 1+sqrt_1_3))

  translated_x <- simple_axes_tbl %>%
    mutate(
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

  expect_error(simple_axes_tbl %>% mutate(bar = foo + c(0, 0, 1, 0)), class = error_class, regexp = message)
  expect_error(simple_axes_tbl %>% mutate(bar = c(0)+ foo), class = error_class, regexp = message)
  expect_error(simple_axes_tbl %>% mutate(bar = foo + c(0, 0)), class = error_class, regexp = message)
  expect_error(simple_axes_tbl %>% mutate(bar = rep(1, 15) + foo), class = error_class, regexp = message)
})

# multiplication test with scalar and vector of scalars


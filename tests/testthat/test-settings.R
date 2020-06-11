context("Settings")

test_that("semantics_axes can be created and accessed", {
  axes <- semantics_axes(y = "up", z = "forward", x = "right", hand = "left")

  expect_equal(axes$up, "+y")
  expect_equal(axes$forward, "+z")
  expect_equal(axes$hand, "left")
  expect_equal(axes$right, "+x")

  axes2 <- semantics_axes(y = "left", x = "down", z = "forward", hand = "right")

  expect_equal(axes2$left, "+y")
  expect_equal(axes2$down, "+x")
  expect_equal(axes2$hand, "right")
  expect_equal(axes2$forward, "+z")
})

test_that("semantics_axes can calculate opposite values", {
  axes <- semantics_axes(y = "up", z = "forward", hand = "left")
  expect_equal(axes$down, "-y")
  expect_equal(axes$backward, "-z")

  axes2 <- semantics_axes(x = "left", z = "down", hand = "left")
  expect_equal(axes2$up, "-z")
  expect_equal(axes2$right, "-x")
})

test_that("semantics_axes can calculate implicit values", {
  axes <- semantics_axes(y = "up", z = "forward", hand = "left")
  expect_equal(axes$right, "+x")
  expect_equal(axes$left, "-x")

  axes2 <- semantics_axes(x = "left", y = "forward", z = "down")
  expect_equal(axes2$hand, "right")

  axes3 <- semantics_axes(x = "left", y = "forward", hand = "left")
  expect_equal(axes3$up, "+z")
  expect_equal(axes3$down, "-z")

  axes4 <- semantics_axes(x = "left", y = "backward", z = "down")
  expect_equal(axes4$hand, "left")
})

test_that("semantics_axes errors out in various ways", {
  error_class <- "dddr_semantics"

  # one of x, y, or z is not a direction
  expect_error(
    semantics_axes(x = "foo", y = "forward", z = "up"),
    class = error_class
  )

  # hand is not correctly specified
  expect_error(
    semantics_axes(x = "up", y = "forward", hand = "gloved"),
    class = error_class
  )

  # missing too many arguments
  expect_error(
    semantics_axes(x = "up", y = "forward"),
    class = error_class
  )

  expect_error(
    semantics_axes(x = "up", hand = "left"),
    class = error_class
  )

  # error when two options are counter to each other
  expect_error(
    semantics_axes(x = "up", y = "down", hand = "left"),
    class = error_class
  )

  expect_error(
    semantics_axes(x = "forward", y = "up", z = "backward"),
    class = error_class
  )

  # error when everything is specified, but incorrectly.
  expect_error(
    semantics_axes(x = "forward", y = "down", z = "left", hand = "left"),
    class = error_class
  )
})

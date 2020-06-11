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

})

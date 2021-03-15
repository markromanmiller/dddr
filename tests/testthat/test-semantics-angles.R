test_that("errors with improper arguments", {

  expect_error(
    semantics_angles(intrinsic = "ypr", extrinsic = "rpy"),
    pattern = "exactly one",
    class = "dddr_semantics"
  )

  expect_error(
    semantics_angles(intrinsic = "ypr", extrinsic = "rpy", hand = "right"),
    pattern = "exactly one",
    class = "dddr_semantics"
  )

  expect_error(
    semantics_angles(hand = "right"),
    pattern = "exactly one",
    class = "dddr_semantics"
  )

  expect_error(
    semantics_angles(),
    pattern = "exactly one",
    class = "dddr_semantics"
  )

  expect_error(
    semantics_angles(extrinsic = "ypr", hand = "robotic"),
    pattern = "must be either",
    class = "dddr_semantics"
  )

  expect_error(
    semantics_angles(intrinsic = "foobar", hand = "left"),
    pattern = "character vector of length 3",
    class = "dddr_semantics"
  )

  expect_error(
    semantics_angles(extrinsic = c("roll", "pitch", "naw"), hand = "right"),
    pattern = '"yaw", "pitch", and "roll"',
    class = "dddr_semantics"
  )

  expect_error(
    semantics_angles(extrinsic = "rpn", hand = "right"),
    pattern = '"yaw", "pitch", and "roll"',
    class = "dddr_semantics"
  )

})

test_that("semantics_angles can be created and accessed", {
  angles <- semantics_angles(intrinsic = "ypr", hand = "left")

  expect_equal(angles$intrinsic[[1]], "yaw")
  expect_equal(angles$intrinsic[2], "pitch")
  expect_equal(angles$intrinsic[[3]], "roll")
  expect_equal(angles$hand, "left")

  angles_2 <- semantics_angles(
    extrinsic = c("pitch", "yaw", "roll"), hand = "right"
  )

  expect_equal(angles_2$extrinsic[1], "pitch")
  expect_equal(angles_2$extrinsic[[2]], "yaw")
  expect_equal(angles_2$hand, "right")
  expect_equal(angles_2$extrinsic[3], "roll")
})

test_that("semantics_angles compute alternate frame correctly", {
  angles <- semantics_angles(extrinsic = "rpy", hand = "left")

  expect_equal(angles$intrinsic[[1]], "yaw")
  expect_equal(angles$intrinsic[2], "pitch")
  expect_equal(angles$intrinsic[[3]], "roll")

  angles_2 <- semantics_angles(
    intrinsic = c("yaw", "roll", "pitch"), hand = "right"
  )

  expect_equal(angles_2$extrinsic[1], "pitch")
  expect_equal(angles_2$extrinsic[[2]], "roll")
  expect_equal(angles_2$extrinsic[3], "yaw")
})

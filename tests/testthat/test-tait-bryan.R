test_that("null rotations have no effect.", {

  set_dddr_semantics(
    axes = semantics_axes_unity,
    angles = semantics_angles(intrinsic = "ypr", hand = "left")
  )
  expect_equal(
    tait_bryan(yaw = 0, pitch = 0, roll = 0),
    quat(1, 0, 0, 0)
  )

  set_dddr_semantics(
    axes = semantics_axes_unity,
    angles = semantics_angles(extrinsic = "pyr", hand = "right")
  )
  expect_equal(
    tait_bryan(yaw = rep(0, 3), pitch = rep(0, 3), roll = rep(0, 3)),
    rep(quat(1, 0, 0, 0), 3)
  )
})


# test single nonzero valyes, those are easiest to think about

test_that("single nonzero values perform sensible rotations", {
  set_dddr_semantics(
    axes = semantics_axes_unity,
    angles = semantics_angles(intrinsic = "ypr", hand = "left")
  )
  expect_equal(
    tait_bryan(yaw = 0, pitch = pi/3, roll = 0),
    quat(cos(pi/6), sin(pi/6), 0, 0)
  )

  set_dddr_semantics(
    axes = semantics_axes_unity,
    angles = semantics_angles(extrinsic = "pyr", hand = "right")
  )
  # Note the opposite hands between rotations and positions.
  expect_equal(
    tait_bryan(yaw = c(pi/3, 0, 0), pitch = c(0, 2*pi/3, 0), roll = c(0, 0, -pi/3)),
    c(
      quat(cos(pi/6), 0, -sin(pi/6), 0),
      quat(cos(pi/3), -sin(pi/3), 0, 0),
      quat(cos(pi/6), 0, 0, sin(pi/6))
    )
  )
})

test_that("multiple nonzero values follow process order", {
  set_dddr_semantics(
    axes = semantics_axes_unity,
    angles = semantics_angles(intrinsic = "ypr", hand = "left")
  )
  expect_equal(
    rotate(vector3(0, 0, 1), tait_bryan(yaw = pi/4, pitch = pi/4, roll = 0)),
    vector3(0.5, -sin(pi/4), 0.5) # this says it's negative.
  )

  set_dddr_semantics(
    axes = semantics_axes(x = "forward", y = "up", hand="left"),
    angles = semantics_angles(extrinsic = "yrp", hand = "right")
  )
  # Note the opposite hands between rotations and positions.
  expect_equal(
    rotate(vector3(1, 0, 0), tait_bryan(yaw = pi/6, pitch = 0, roll = -pi/6)),
    vector3(cos(pi/6), -sin(pi/6)^2, cos(pi/6)*sin(pi/6))
  )
})


set_dddr_semantics(axes = NULL, angles = NULL)



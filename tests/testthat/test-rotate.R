foo <- vector3(
  x = c(1, 0, 0, -4, 0, 0),
  y = c(0, 2, 0, 0, -5, 0),
  z = c(0, 0, 3, 0, 0, -6)
)

rh <- sqrt(2) / 2 # root-half, the sqrt of one-half

xp90 <- quat(rh, rh, 0, 0)
xn90 <- quat(rh, -rh, 0, 0)

test_that("Simple rotations", {
  # simplest I could think of
  expect_equal(
    rotate(foo, xp90),
    vector3(
      x = foo$x,
      y = -foo$z,
      z = foo$y
    )
  )

  # negative rotation
  expect_equal(
    rotate(foo, xn90),
    vector3(
      x = foo$x,
      y = foo$z,
      z = -foo$y
    )
  )

  # equivalent to xp90 by double cover
  expect_equal(
    rotate(foo, quat(-rh, -rh, 0, 0)),
    rotate(foo, xp90)
  )

  # back and forth produces no effect.
  expect_equal(rotate(rotate(foo, xp90), xn90), foo)
  expect_equal(rotate(rotate(foo, xn90), xp90), foo)
  expect_equal(rotate(foo, xn90 * xp90), foo)
})

test_that("rotations can be expressed by axis and angle", {
  expect_equal(
    rotate(foo, axis = vector3(0, 1, 0), angle = pi / 2),
    vector3(
      x = foo$z,
      y = foo$y,
      z = -foo$x
    )
  )

  expect_equal(
    rotate(foo, axis = vector3(0, 1, 0), angle = -pi / 2),
    vector3(
      x = -foo$z,
      y = foo$y,
      z = foo$x
    )
  )

  expect_equal(
    rotate(foo, axis = vector3(0, 1, 0), angle = 2 * pi + pi / 2),
    # same as if it's pi/4
    vector3(
      x = foo$z,
      y = foo$y,
      z = -foo$x
    )
  )
})

test_that("rotations can be offset by origin", {
  expect_equal(
    rotate(foo, xp90, origin = vector3(0, 1, 0)),
    vector3(
      x = foo$x,
      y = c(1, 1, -2, 1, 1, 7),
      z = c(-1, 1, -1, -1, -6, -1)
    )
  )
})

test_that("rotations can be specified by from-to", {
  expect_equal(
    rotate(foo, from = vector3(1, 0, 0), to = vector3(0, 1, 0)),
    vector3(
      x = -foo$y,
      y = foo$x,
      z = foo$z
    )
  )

  expect_equal(
    rotate(foo, from = vector3(2, 0, 0), to = vector3(0, 0, -3)),
    vector3(
      x = foo$z,
      y = foo$y,
      z = -foo$x
    )
  )
})

test_that("rotations can be specified by axis and from-to", {
  expect_equal(
    rotate(foo,
           axis = vector3(0, 0, 2),
           from = vector3(1, 0, 2),
           to = vector3(0, 1, -5)
    ),
    vector3(
      x = -foo$y,
      y = foo$x,
      z = foo$z
    )
  )
})

test_that("errors are thrown in reasonable cases", {
  # everything is null
  expect_error(rotate(foo))

  # axis is given but no angle and no from-to
  expect_error(rotate(foo, axis = vector3(0, 0, 1)))

  # axis and from is given but no to
  expect_error(rotate(foo, axis = vector3(0, 0, 1), from = vector3(1, 0, 1)))
})

test_that("warnings are thrown in reasonable cases", {
  # rotator and anything else
  expect_warning(rotate(foo, quat(1, 0, 0, 0), angle = pi / 2))

  # angle, axis, and from/to are given.
  expect_warning(rotate(foo,
                        angle = pi / 2,
                        axis = vector3(0, 1, 0),
                        from = vector3(2, 3, 4),
                        to = vector3(-2, 5, 6)))
})


test_that("yaw works correctly in default system", {
  radian_measures <- seq(0, 360, 10) / 180 * pi
  expect_equal(
    yaw(quat(
      w = cos(radian_measures / 2),
      x = 0,
      y = sin(radian_measures / 2),
      z = 0
    )),
    c(seq(0, 180, 10), seq(-170, 0, 10)) / 180 * pi
  )
})

test_that("pitch works correctly in default system", {
  radian_measures <- seq(-90, 90, by = 10) / 180 * pi
  expect_equal(
    pitch(quat(
      w = cos(radian_measures / 2),
      x = sin(radian_measures / 2),
      y = 0,
      z = 0
    )),
    # In unity, Z tilting up means NEGATIVE pitch!!
    # This is why references are so good and so necessary
    seq(-90, 90, by = 10) / 180 * pi
  )
})

test_that("roll works correctly in default system", {
  radian_measures <- seq(-180, 180, 10) / 180 * pi
  expect_equal(
    roll(quat(
      w = cos(radian_measures / 2),
      x = 0,
      y = 0,
      z = sin(radian_measures / 2)
    )),
    radian_measures
  )
})

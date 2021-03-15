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
    tait_bryan(yaw = 0, pitch = pi / 3, roll = 0),
    quat(cos(pi / 6), sin(pi / 6), 0, 0)
  )

  set_dddr_semantics(
    axes = semantics_axes_unity,
    angles = semantics_angles(extrinsic = "pyr", hand = "right")
  )
  # Note the opposite hands between rotations and positions.
  expect_equal(
    tait_bryan(
      yaw = c(pi / 3, 0, 0),
      pitch = c(0, 2 * pi / 3, 0),
      roll = c(0, 0, -pi / 3)
    ),
    c(
      quat(cos(pi / 6), 0, -sin(pi / 6), 0),
      quat(cos(pi / 3), -sin(pi / 3), 0, 0),
      quat(cos(pi / 6), 0, 0, sin(pi / 6))
    )
  )
})

test_that("multiple nonzero values follow process order", {
  set_dddr_semantics(
    axes = semantics_axes_unity,
    angles = semantics_angles(intrinsic = "ypr", hand = "left")
  )
  expect_equal(
    rotate(
      vector3(0, 0, 1),
      tait_bryan(yaw = pi / 4, pitch = pi / 4, roll = 0)
    ),
    vector3(0.5, -sin(pi / 4), 0.5) # this says it's negative.
  )

  set_dddr_semantics(
    axes = semantics_axes(x = "forward", y = "up", hand = "left"),
    angles = semantics_angles(extrinsic = "yrp", hand = "right")
  )
  # Note the opposite hands between rotations and positions.
  expect_equal(
    rotate(
      vector3(1, 0, 0),
      tait_bryan(yaw = pi / 6, pitch = 0, roll = -pi / 6)
    ),
    vector3(cos(pi / 6), -sin(pi / 6)^2, cos(pi / 6) * sin(pi / 6))
  )
})

# for the loop, use this syntax: show_failure(expect_equal(f(!!i), !!(i * 10)))

f1 <- function(x, y, z, a) {
  x
}

test_that("ypr goes back and forth with the right conventions", {
  # if you're running this test, you might get less of a deluge
  # if you limit yourself to one hand,
  # one axes system,
  # one angle version,
  # and lower point density.

  angle_versions <- c("pry", "pyr", "rpy", "ryp", "ypr", "yrp")
  hand <- c("left", "right")
  axes_versions <- c("unreal", "unity", "opengl")

  pd <- 5 # short for point density

  # use an odd divisor to avoid gimbal lock
  over_pi_angles <- pi * seq(-pd, pd) / (2 * pd + 1)
  over_2pi_angles <- pi * seq(-2 * pd, 2 * pd) / (2 * pd + 1)

  for (ax in axes_versions) {

    axsem <- switch(
      ax,
      unity = semantics_axes_unity,
      unreal = semantics_axes_unreal,
      opengl = semantics_axes_opengl
    )

    for (angs in angle_versions) {
      for (h in hand) {
        angsem <- semantics_angles(extrinsic = angs, hand = h)
        set_dddr_semantics(angle = angsem, axes = axsem)

        yaw_angles <- if (angsem$extrinsic[[2]] == "yaw") {
          over_pi_angles
        } else {
          over_2pi_angles
        }

        pitch_angles <- if (angsem$extrinsic[[2]] == "pitch") {
          over_pi_angles
        } else {
          over_2pi_angles
        }

        roll_angles <- if (angsem$extrinsic[[2]] == "roll") {
          over_pi_angles
        } else {
          over_2pi_angles
        }

        ypr <- expand.grid(
          yaw = yaw_angles,
          pitch = pitch_angles,
          roll = roll_angles
        )
        tb <- tait_bryan(yaw = ypr$yaw, pitch = ypr$pitch, roll = ypr$roll)

        expect_equal(f1(yaw(tb), !!angs, !!ax, !!h), ypr$yaw)
        expect_equal(f1(pitch(tb), !!angs, !!ax, !!h), ypr$pitch)
        expect_equal(f1(roll(tb), !!angs, !!ax, !!h), ypr$roll)
      }
    }
  }

})

set_dddr_semantics(axes = NULL, angles = NULL)

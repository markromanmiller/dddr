# angle_between
test_that("angle_between calculates simple angles", {
  expect_equal(angle_between(vector3(1, 0, 0), vector3(0, 1, 0)), pi / 2)
  expect_equal(angle_between(vector3(1, 0, 0), vector3(0, -1, 0)), pi / 2)
  expect_equal(angle_between(vector3(1, 0, 0), vector3(1, 0, 0)), 0)
  expect_equal(angle_between(vector3(1, 0, 0), vector3(2.5, -2.5, 0)), pi / 4)
})

test_that("angle_between calculates simple angles around origin", {
  expect_equal(
    angle_between(
      vector3(1, 0, 0),
      vector3(0, 1, 0),
      origin = vector3(1, 1, 0)
    ),
    pi / 2
  )
  expect_equal(
    angle_between(
      vector3(1, 0, 0),
      vector3(0, 0, 0),
      origin = vector3(0, -1, 0)
    ),
    pi / 4
  )
})

# project
test_that("project works correctly", {
  expect_equal(project(vector3(2, 3, 4), vector3(0, 1, 0)), vector3(0, 3, 0))
  expect_equal(project(vector3(2, 3, 4), vector3(2, 0, 0)), vector3(2, 0, 0))
  expect_equal(
    project(vector3(2, 3, 4), vector3(0, 1, 1)),
    vector3(0, 3.5, 3.5)
  )
})

# reject
test_that("vector rejection is sensible", {
  expect_equal(reject(vector3(2, 3, 4), vector3(0, 1, 0)), vector3(2, 0, 4))
  expect_equal(reject(vector3(2, 3, 4), vector3(2, 0, 0)), vector3(0, 3, 4))
  expect_equal(reject(vector3(2, 3, 4), vector3(0, 1, 1)), vector3(2, -.5, .5))
})

# reinterpret
test_that("vector reinterpretation errors", {
  # error when too few options are given.
  expect_error(
    reinterpret(vector3(1, 0, 0), new_x_from = vector3(0, 0, 1)),
    "Not enough"
  )

  # error when contradictory options are given.
  expect_error(
    reinterpret(
      vector3(1, 0, 0),
      new_x_from = vector3(0, 0, 1),
      new_y_from = vector3(0, 1, 0),
      new_z_from = vector3(0, 0, 1)
    ),
    "not orthogonal"
  )

  # error when one of each (new and old) is given.
  expect_error(
    reinterpret(
      vector3(1, 0, 0),
      new_x_from = vector3(0, 0, 1), old_y_to = vector3(0, 1, 0)
    ),
    "old and new"
  )
})

test_that("vector reinterpretation computes and reverses", {
  # simple test....
  old_x_in_new_basis <- vector3(sqrt(1 / 2), sqrt(1 / 2), 0)
  old_y_in_new_basis <- vector3(sqrt(1 / 3), -sqrt(1 / 3), -sqrt(1 / 3))
  old_z_in_new_basis <- vector3(-sqrt(1 / 6), sqrt(1 / 6), -sqrt(2 / 3))

  expect_equal(
    reinterpret(
      vector3(1, 3, 4),
      old_x_to = old_x_in_new_basis, old_y_to = old_y_in_new_basis
    ),
    old_x_in_new_basis + 3 * old_y_in_new_basis + 4 * old_z_in_new_basis
  )

  # and do the reverse...
  expect_equal(
    reinterpret(
      old_x_in_new_basis + 3 * old_y_in_new_basis + 4 * old_z_in_new_basis,
      new_x_from = old_x_in_new_basis, new_y_from = old_y_in_new_basis
    ),
    vector3(1, 3, 4)
  )
})

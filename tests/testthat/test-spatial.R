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

test_that("angle_between tolerates error near machine precision", {
  # when I was running an analysis with natural data,
  # I received NaNs in the analysis. This was the value that produced an error.
  # In short, the argument to `acos` was ever so slightly larger than 1.
  special_case <- vector3(
    -0.36681549344168018489,
    -0.02235231534128476541,
    0.93002514362248289714
  )
  expect_equal(angle_between(special_case, special_case), 0, tolerance = 2e-8)
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

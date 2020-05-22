context("Spatial tasks")

# angle_between
test_that("angle_between calculates simple angles", {
  expect_equal(angle_between(vector3(1, 0, 0), vector3(0, 1, 0)), pi/2)
  expect_equal(angle_between(vector3(1, 0, 0), vector3(0, -1, 0)), pi/2)
  expect_equal(angle_between(vector3(1, 0, 0), vector3(1, 0, 0)), 0)
  expect_equal(angle_between(vector3(1, 0, 0), vector3(2.5, -2.5, 0)), pi/4)
})

test_that("angle_between calculates simple angles around origin", {
  expect_equal(angle_between(vector3(1, 0, 0), vector3(0, 1, 0), origin=c(1, 1, 0)), pi/2)
  expect_equal(angle_between(vector3(1, 0, 0), vector3(0, 0, 0), origin=c(0, -1, 0)), pi/4)
})

# project

# reject

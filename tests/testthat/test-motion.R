foo <- vector3(
  x = c(1, 0, 0, -4, 0, 0),
  y = c(0, 2, 0, 0, -5, 0),
  z = c(0, 0, 3, 0, 0, -6)
)

test_that("linear approximations work", {
  times <- c(1.3, 4.5, 5.8)
  foo_approx <- approx(1:6, foo, times, n = 50)$y

  expect_equal(foo_approx$x, c(0.7, -2  ,  0  ))
  expect_equal(foo_approx$y, c(0.6, -2.5, -1  ))
  expect_equal(foo_approx$z, c(0  ,  0  , -4.8))
})

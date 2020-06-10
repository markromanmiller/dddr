simple_vector <- vector3(1:4, 2:5, 3:6)

test_that("dplyr plays nicely with vector3s", {
  dplyr_test <- dplyr::mutate(
    data.frame(simple_vector = simple_vector),
    simple_vector = simple_vector + vector3(0, 1, 0)
  )

  expect_equal(dplyr_test$simple_vector$x, 1:4)
  expect_equal(dplyr_test$simple_vector$y, 3:6)
  expect_equal(dplyr_test$simple_vector$z, 3:6)
})

test_that("lead / lag work sensibly", {
  lag_entry <- c(NA, 1, 1, 1)
  lead_entry <- c(-1, -1, -1, NA)

  expect_equal(
    simple_vector - dplyr::lag(simple_vector),
    vector3(
      x = lag_entry,
      y = lag_entry,
      z = lag_entry
    )
  )

  expect_equal(
    simple_vector - dplyr::lead(simple_vector),
    vector3(
      x = lead_entry,
      y = lead_entry,
      z = lead_entry
    )
  )
})

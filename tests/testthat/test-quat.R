context("quat basics")

s <- sqrt(1 / 2)

foo_qw <- c(1, s, s, s, 0, 0, 0)
foo_qx <- c(0, s, 0, 0, 1, 0, 0)
foo_qy <- c(0, 0, s, 0, 0, 1, 0)
foo_qz <- c(0, 0, 0, s, 0, 0, 1)

simple_quat_tbl <- data.frame(foo = quat(w = foo_qw, x = foo_qx, y = foo_qy, z = foo_qz))

test_that("Quat entries can be extracted", {
  extracted <- dplyr::mutate(
    simple_quat_tbl,
    foo_qw = foo$w,
    foo_qx = foo$x,
    foo_qy = foo$y,
    foo_qz = foo$z
  )
  expect_equal(extracted$foo_qw, foo_qw)
  expect_equal(extracted$foo_qx, foo_qx)
  expect_equal(extracted$foo_qy, foo_qy)
  expect_equal(extracted$foo_qz, foo_qz)
})

test_that("Quat entries can be conjugated", {
  conjugated <- dplyr::mutate(simple_quat_tbl,
    foo = Conj(foo),
    foo_qw = foo$w,
    foo_qx = foo$x,
    foo_qy = foo$y,
    foo_qz = foo$z
  )
  expect_equal(conjugated$foo_qw, foo_qw)
  expect_equal(conjugated$foo_qx, -foo_qx)
  expect_equal(conjugated$foo_qy, -foo_qy)
  expect_equal(conjugated$foo_qz, -foo_qz)
})

test_that("The product of a quat and its conjugate is the identity", {
  cancelled <- dplyr::mutate(simple_quat_tbl,
    bar = foo * Conj(foo),
    bar_qw = bar$w,
    bar_qx = bar$x,
    bar_qy = bar$y,
    bar_qz = bar$z
  )

  n_entries <- length(cancelled$bar)

  expect_equal(cancelled$bar_qw, rep(1, n_entries))
  expect_equal(cancelled$bar_qx, rep(0, n_entries))
  expect_equal(cancelled$bar_qy, rep(0, n_entries))
  expect_equal(cancelled$bar_qz, rep(0, n_entries))
})

test_that("Quats can be multiplied by quats", {
  multiplied <- dplyr::mutate(simple_quat_tbl,
    bar = foo * foo
  )

  bar_expected <- quat(
    w = c(1, 0, 0, 0, 1, 1, 1),
    x = c(0, 1, 0, 0, 0, 0, 0),
    y = c(0, 0, 1, 0, 0, 0, 0),
    z = c(0, 0, 0, 1, 0, 0, 0)
  )

  expect_equal(multiplied$bar, bar_expected)
})

test_that("Quats can be multiplied by a 4-element numeric representing an identity quat", {
  multiplied_broadcast <- dplyr::mutate(simple_quat_tbl,
    bar = foo * c(1, 0, 0, 0)
  )
  expect_equal(multiplied_broadcast$bar, multiplied_broadcast$foo)
})

# test_that("Quats multiply according to ijk = -1", {
#  units <- tibble(
#    names = c("1", "i", "j", "k"),
#    quats = quat(
#      w = c(1, 0, 0, 0),
#      x = c(0, 1, 0, 0),
#      y = c(0, 0, 1, 0),
#      z = c(0, 0, 0, 1)
#    )
#  )

#  unit_pairs <- units %>%
#    rename_with(~paste0(..1, "_left")) %>%
#    crossing(units %>% rename_with(~paste0(..1, "_right")))

#  results <- tribble(
#    ~names_left, ~names_right, ~names_expected, ~quats_expected,
#    "1", "1", "1", upgrade_to_quat(c(1, 0, 0, 0)),
#    "1", "i", "i", upgrade_to_quat(c(0, 1, 0, 0)),
#    "1", "j", "j", upgrade_to_quat(c(0, 0, 1, 0)),
#    "1", "k", "k", upgrade_to_quat(c(0, 0, 0, 1)),

#   "i", "1", "i", upgrade_to_quat(c(0, 1, 0, 0)),
#    "i", "i", "-1", upgrade_to_quat(c(-1, 0, 0, 0)),
#    "i", "j", "k", upgrade_to_quat(c(0, 0, 0, 1)),
#    "i", "k", "-j", upgrade_to_quat(c(0, 0, -1, 0)),

#    "j", "1", "j", upgrade_to_quat(c(0, 0, 1, 0)),
#    "j", "i", "-k", upgrade_to_quat(c(0, 0, 0, -1)),
#    "j", "j", "-1", upgrade_to_quat(c(-1, 0, 0, 0)),
#    "j", "k", "i", upgrade_to_quat(c(0, 1, 0, 0)),

#    "k", "1", "k", upgrade_to_quat(c(0, 0, 0, 1)),
#    "k", "i", "j", upgrade_to_quat(c(0, 0, 1, 0)),
#    "k", "j", "-i", upgrade_to_quat(c(0, -1, 0, 0)),
#    "k", "k", "-1", upgrade_to_quat(c(-1, 0, 0, 0)),
#  ) %>%
#    unnest(quats_expected)

#  ijk_test <- unit_pairs %>%
#   left_join(results, by=c("names_left", "names_right")) %>%
#   mutate(
#      quats_actual = quats_left * quats_right,
#      aw = quats_actual$w,
#      ax = quats_actual$x,
#      ay = quats_actual$y,
#      az = quats_actual$z,
#     ew = quats_expected$w,
#      ex = quats_expected$x,
#      ey = quats_expected$y,
#      ez = quats_expected$z,
#   )

#  expect_equal(ijk_test$aw, ijk_test$ew)
#  expect_equal(ijk_test$ax, ijk_test$ex)
#  expect_equal(ijk_test$ay, ijk_test$ey)
#  expect_equal(ijk_test$az, ijk_test$ez)
# })

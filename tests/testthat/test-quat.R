context("quat basics")

s <- sqrt(1 / 2)

foo_qw <- c(1, s, s, s, 0, 0, 0)
foo_qx <- c(0, s, 0, 0, 1, 0, 0)
foo_qy <- c(0, 0, s, 0, 0, 1, 0)
foo_qz <- c(0, 0, 0, s, 0, 0, 1)

simple_quat_tbl <- data.frame(
  foo = quat(
    w = foo_qw,
    x = foo_qx,
    y = foo_qy,
    z = foo_qz
  )
)

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

test_that(
  paste(
    "Quats can be multiplied by a 4-element numeric",
    "representing an identity quat"
  ), {
  multiplied_broadcast <- dplyr::mutate(simple_quat_tbl,
    bar = foo * c(1, 0, 0, 0)
  )
  expect_equal(multiplied_broadcast$bar, multiplied_broadcast$foo)
})

test_that("quat formatting", {
  expect_known_output(
    pillar::pillar_shaft(simple_quat_tbl$foo) %>%
      format(width = 34) %>%
      print(),
    file = "pillar_shaft_quat_print_test_34.out"
  )
})

test_that("Quats multiply according to ijk = -1", {

  `1` <- quat(1, 0, 0, 0)
  `i` <- quat(0, 1, 0, 0)
  `j` <- quat(0, 0, 1, 0)
  `k` <- quat(0, 0, 0, 1)

  `-1` <- quat(-1, 0, 0, 0)
  `-i` <- quat(0, -1, 0, 0)
  `-j` <- quat(0, 0, -1, 0)
  `-k` <- quat(0, 0, 0, -1)

  expect_equal(`1` * `1`, `1`)
  expect_equal(`1` * `i`, `i`)
  expect_equal(`1` * `j`, `j`)
  expect_equal(`1` * `k`, `k`)

  expect_equal(`i` * `1`, `i`)
  expect_equal(`i` * `i`, `-1`)
  expect_equal(`i` * `j`, `k`)
  expect_equal(`i` * `k`, `-j`)

  expect_equal(`j` * `1`, `j`)
  expect_equal(`j` * `i`, `-k`)
  expect_equal(`j` * `j`, `-1`)
  expect_equal(`j` * `k`, `i`)

  expect_equal(`k` * `1`, `k`)
  expect_equal(`k` * `i`, `j`)
  expect_equal(`k` * `j`, `-i`)
  expect_equal(`k` * `k`, `-1`)

})

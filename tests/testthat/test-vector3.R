context("vector3 basics")

sqrt_1_3 <- sqrt(1 / 3)

foo_px <- c(1, 0, 0, sqrt_1_3)
foo_py <- c(0, 1, 0, sqrt_1_3)
foo_pz <- c(0, 0, 1, sqrt_1_3)

simple_axes_tbl <- data.frame(foo = vector3(x = foo_px, y = foo_py, z = foo_pz))

test_that("Vector3 entries can be extracted", {
  extracted <- dplyr::mutate(simple_axes_tbl,
    foo_px = foo$x,
    foo_py = foo$y,
    foo_pz = foo$z
  )
  expect_equal(extracted$foo_px, foo_px)
  expect_equal(extracted$foo_py, foo_py)
  expect_equal(extracted$foo_pz, foo_pz)
})

test_that("Vector3 entries can be added", {
  doubled <- dplyr::mutate(simple_axes_tbl,
    bar = foo + foo,
    bar_px = bar$x,
    bar_py = bar$y,
    bar_pz = bar$z
  )

  bar_px <- c(2, 0, 0, 2 * sqrt_1_3)
  bar_py <- c(0, 2, 0, 2 * sqrt_1_3)
  bar_pz <- c(0, 0, 2, 2 * sqrt_1_3)

  expect_equal(doubled$bar_px, bar_px)
  expect_equal(doubled$bar_py, bar_py)
  expect_equal(doubled$bar_pz, bar_pz)
})

test_that(
  paste(
    "Vector3 entries can be added and subtracted to",
    "numeric vectors of length 3"
  ), {
  translated_z <- dplyr::mutate(simple_axes_tbl,
    bar = foo + c(0, 0, 1),
    bar_px = bar$x,
    bar_py = bar$y,
    bar_pz = bar$z
  )
  expect_equal(translated_z$bar_px, foo_px)
  expect_equal(translated_z$bar_py, foo_py)
  expect_equal(translated_z$bar_pz, c(1, 1, 2, 1 + sqrt_1_3))

  translated_x <- dplyr::mutate(simple_axes_tbl,
    bar = c(1, 0, 0) + foo,
    bar_px = bar$x,
    bar_py = bar$y,
    bar_pz = bar$z
  )
  expect_equal(translated_x$bar_px, c(2, 1, 1, 1 + sqrt_1_3))
  expect_equal(translated_x$bar_py, foo_py)
  expect_equal(translated_x$bar_pz, foo_pz)

  translated_x <- dplyr::mutate(simple_axes_tbl,
    bar = c(0, 1, 0) - foo,
    bar_px = bar$x,
    bar_py = bar$y,
    bar_pz = bar$z
  )
  expect_equal(translated_x$bar_px, -foo_px)
  expect_equal(translated_x$bar_py, c(1, 0, 1, 1 - sqrt_1_3))
  expect_equal(translated_x$bar_pz, -foo_pz)
})

test_that(
  "Vector3 entries can't be added to numeric vectors of various non-3 sizes", {
    error_class <- "dplyr_error"
    message <- "To add or subtract a numeric and a vector3,"

    expect_error(
      dplyr::mutate(simple_axes_tbl, bar = foo + c(0, 0, 1, 0)),
      class = error_class,
      regexp = message
    )
    expect_error(
      dplyr::mutate(simple_axes_tbl, bar = c(0) + foo),
      class = error_class,
      regexp = message
    )
    expect_error(
      dplyr::mutate(simple_axes_tbl, bar = foo + c(0, 0)),
      class = error_class,
      regexp = message
    )
    expect_error(
      dplyr::mutate(simple_axes_tbl, bar = rep(1, 15) + foo),
      class = error_class,
      regexp = message
    )
  }
)

test_that(
  "Vector3 can be multiplied both left and right by numeric of length 1", {
    doubled <- dplyr::mutate(simple_axes_tbl,
      bar = foo * 2,
      baz = 3 * foo
    )

    expect_equal(doubled$bar$x, c(2, 0, 0, 2 * sqrt_1_3))
    expect_equal(doubled$bar$y, c(0, 2, 0, 2 * sqrt_1_3))
    expect_equal(doubled$bar$z, c(0, 0, 2, 2 * sqrt_1_3))

    expect_equal(doubled$baz$x, c(3, 0, 0, 3 * sqrt_1_3))
    expect_equal(doubled$baz$y, c(0, 3, 0, 3 * sqrt_1_3))
    expect_equal(doubled$baz$z, c(0, 0, 3, 3 * sqrt_1_3))
  }
)

test_that(
  paste(
    "Vector3 can be multiplied both left and right by numeric vector",
    "the same length as the vector3 vector"
  ), {
    doubled <- dplyr::mutate(simple_axes_tbl,
      fac = seq(1, 7, by = 2),
      bar = foo * c(1, 2, 3, 4),
      baz = fac * foo
    )

    expect_equal(doubled$bar$x, c(1, 0, 0, 4 * sqrt_1_3))
    expect_equal(doubled$bar$y, c(0, 2, 0, 4 * sqrt_1_3))
    expect_equal(doubled$bar$z, c(0, 0, 3, 4 * sqrt_1_3))

    expect_equal(doubled$baz$x, c(1, 0, 0, 7 * sqrt_1_3))
    expect_equal(doubled$baz$y, c(0, 3, 0, 7 * sqrt_1_3))
    expect_equal(doubled$baz$z, c(0, 0, 5, 7 * sqrt_1_3))
  }
)

test_that("Vector3 distances are correctly computed", {
  distances <- dplyr::mutate(data.frame(
    foo = vector3(
      x = c(-.5, .5, sqrt(2) / 2, 3),
      y = c(-.5, -sqrt(2) / 2, .5, 4),
      z = c(sqrt(2) / 2, .5, -.5, 0)
    )
  ),
  distance = distance(foo)
  )

  expect_equal(distances$distance, c(1, 1, 1, 5))
})

test_that("Vector3 distances are correctly computed from an offset", {
  distances <- dplyr::mutate(data.frame(
    foo = vector3(
      x = c(1, -4, 4, 3),
      y = c(0, -3, 5, 4),
      z = c(-2, 5, -7, 0)
    )
  ),
  distance = distance(foo, from = c(0, 2, -3))
  )

  expect_equal(distances$distance, c(sqrt(6), sqrt(105), sqrt(41), sqrt(22)))
})

test_that("Vector3 can be normalized", {
  normalized <- dplyr::mutate(data.frame(
    foo = vector3(
      x = c(1, 0, 4, 3),
      y = c(0, 0, 5, 4),
      z = c(-1, 5, -7, 0)
    )
  ),
  unit = normalize(foo),
  normalized_distance = normalize(foo) %>% distance(),
  normalized_distance_of_5 = normalize(foo, length = 5) %>% distance()
  )

  expect_equal(head(normalized$unit$x, 2), c(sqrt(2) / 2, 0))
  expect_equal(head(normalized$unit$y, 2), c(0, 0))
  expect_equal(head(normalized$unit$z, 2), c(-sqrt(2) / 2, 1))

  expect_equal(normalized$normalized_distance, c(1, 1, 1, 1))
  expect_equal(normalized$normalized_distance_of_5, c(5, 5, 5, 5))
})

test_that("Vector3 can have a unary negative", {
  negated <- dplyr::mutate(
    simple_axes_tbl,
    neg = -foo,
    cancel = foo + neg
  )

  expect_equal(negated$neg$x, -foo_px)
  expect_equal(negated$neg$y, -foo_py)
  expect_equal(negated$neg$z, -foo_pz)
  expect_equal(negated$cancel, rep(vector3(0, 0, 0), 4))
})

test_that("Vector3 can have sums, cumsums, and means", {
  sum_test <- dplyr::mutate(
    data.frame(foo = vector3(
      x = c(1, 0, -1, 5),
      y = c(4, -2, 0, 1),
      z = c(-1, 2, 4, 3)
    )),
    sum_foo = sum(foo),
    cumsum_foo = cumsum(foo),
    mean_foo = mean(foo)
  )

  expect_equal(sum_test$sum_foo$x, rep(5, 4))
  expect_equal(sum_test$sum_foo$y, rep(3, 4))
  expect_equal(sum_test$sum_foo$z, rep(8, 4))

  expect_equal(sum_test$cumsum_foo$x, c(1, 1, 0, 5))
  expect_equal(sum_test$cumsum_foo$y, c(4, 2, 2, 3))
  expect_equal(sum_test$cumsum_foo$z, c(-1, 1, 5, 8))

  expect_equal(sum_test$mean_foo$x, rep(5 / 4, 4))
  expect_equal(sum_test$mean_foo$y, rep(3 / 4, 4))
  expect_equal(sum_test$mean_foo$z, rep(8 / 4, 4))
})

test_that("Vector3 dot product behaves correctly.", {
  foo <- vector3(
    x = c(1, 0, -1, 5),
    y = c(4, -2, 0, 1),
    z = c(-1, 2, 4, 3)
  )
  # simple
  expect_equal(dot(foo, vector3(1, 0, 0)), c(1, 0, -1, 5))
  # slightly more complex
  expect_equal(dot(foo, vector3(-1, 2, 0)), c(7, -4, 1, -3))
  # dot product with self is squared length
  expect_equal(dot(foo, foo), distance(foo)^2)

  # also do dots with length-3 numerics
  expect_equal(dot(foo, c(1, 0, 0)), c(1, 0, -1, 5))
})

test_that("Vector3 cross product behaves correctly.", {
  foo <- vector3(
    x = c(1, 0, -1, 5),
    y = c(4, -2, 0, 1),
    z = c(-1, 2, 4, 3)
  )
  # simple
  expect_equal(
    cross(vector3(1, 0, 0), vector3(0, 1, 0)),
    vector3(0, 0, 1)
  )
  # anticommutativity
  expect_equal(
    cross(vector3(0, 1, 0), vector3(1, 0, 0)),
    vector3(0, 0, -1)
  )

  crossed_with_x <- vector3(
    x = c(0, 0, 0, 0), # 0,
    y = c(-1, 2, 4, 3), # z,
    z = c(-4, 2, 0, -1) # -y
  )

  # slightly more complex
  expect_equal(cross(foo, vector3(1, 0, 0)), crossed_with_x)

  # also do dots with length-3 numerics
  expect_equal(cross(foo, c(1, 0, 0)), crossed_with_x)

  # dot between result and either input is 0
  dot_test_vec <- vector3(-2.5, 4, 1)
  crossed <- cross(foo, dot_test_vec)
  expect_equal(dot(crossed, foo), rep(0, 4))
  expect_equal(dot(crossed, dot_test_vec), rep(0, 4))
})

test_that(
  paste(
    "When multiplying vectors, the user gets an error",
    "and redirects to the operations page."
  ),
  expect_error(
    vector3(1, 0, 0) * vector3(1, 0, 0),
    class = "vctrs_error",
    regexp = "vector3_prod"
  )
)

test_that("format and print work sensibly", {
  print_test <- vector3(
    x = cos(seq(0, 4 * pi, by = 0.5)),
    y = sin(seq(0, 4 * pi, by = 0.5)),
    z = exp(seq(0, 4 * pi, by = 0.5))
  )

  expect_known_output(
    print(print_test),
    file = "print_vector3_print_test.out"
  )

  expect_known_output(
    pillar::pillar_shaft(print_test) %>% format(width = 27) %>% print(),
    file = "pillar_shaft_vector3_print_test_27.out"
  )
  expect_known_output(
    pillar::pillar_shaft(print_test) %>% format(width = 28) %>% print(),
    file = "pillar_shaft_vector3_print_test_28.out"
  )
  expect_known_output(
    pillar::pillar_shaft(print_test) %>% format(width = 29) %>% print(),
    file = "pillar_shaft_vector3_print_test_29.out"
  )
  expect_known_output(
    pillar::pillar_shaft(print_test) %>% format(width = 30) %>% print(),
    file = "pillar_shaft_vector3_print_test_30.out"
  )
  expect_known_output(
    pillar::pillar_shaft(print_test) %>% format(width = 35) %>% print(),
    file = "pillar_shaft_vector3_print_test_35.out"
  )
})

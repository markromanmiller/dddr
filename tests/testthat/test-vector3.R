sqrt_1_3 <- sqrt(1 / 3)

foo_px <- c(1, 0, 0, sqrt_1_3)
foo_py <- c(0, 1, 0, sqrt_1_3)
foo_pz <- c(0, 0, 1, sqrt_1_3)

simple_vector <- vector3(1:4, 2:5, 3:6)

foo <- vector3(x = foo_px, y = foo_py, z = foo_pz)

test_that("Vector3 entries can be extracted", {
  expect_equal(foo$x, foo_px)
  expect_equal(foo$y, foo_py)
  expect_equal(foo$z, foo_pz)
})

test_that("Vector3 entries can be added", {
  doubled <- foo + foo

  doubled_px <- c(2, 0, 0, 2 * sqrt_1_3)
  doubled_py <- c(0, 2, 0, 2 * sqrt_1_3)
  doubled_pz <- c(0, 0, 2, 2 * sqrt_1_3)

  expect_equal(doubled$x, doubled_px)
  expect_equal(doubled$y, doubled_py)
  expect_equal(doubled$z, doubled_pz)
})

test_that("Vector3 entries can be added if either is length 1", {
  a <- vector3(1, 2, 3)
  b <- vector3(1:4, 2:5, 3:6)
  s <- vector3(2:5, 4:7, 6:9)

  expect_equal(a + b, s)
  expect_equal(b + a, s)
})

test_that("Vector3 entries can be subtracted if either is length 1", {
  a <- vector3(1, 2, 3)
  b <- vector3(1:4, 2:5, 3:6)
  s <- vector3(0:3, 0:3, 0:3)

  expect_equal(b - a, s)
  expect_equal(a - b, -s)
})

test_that("Vector3 entries can't be added if the length does not match", {
  a <- vector3(1:2, 2:3, 3:4)
  b <- vector3(1:4, 2:5, 3:6)

  expect_error(a + b, class = "vctrs_error_incompatible_size")
  expect_error(b + a, class = "vctrs_error_incompatible_size")
})

test_that(
  "Vector3 entries can't be added to numeric vectors", {
    error_class <- "vctrs_error_incompatible_op"
    message <- "vector3_arith"

    expect_error(
      foo + c(0, 0, 1, 0),
      class = error_class,
      regexp = message
    )
    expect_error(
      c(0) + foo,
      class = error_class,
      regexp = message
    )
    expect_error(
      foo + c(0, 0, 1),
      class = error_class,
      regexp = message
    )
    expect_error(
      rep(1, 15) + foo,
      class = error_class,
      regexp = message
    )
  }
)

test_that(
  "Vector3 can be multiplied both left and right by numeric of length 1", {
    bar <- foo * 2
    baz <- 3 * foo

    expect_equal(bar$x, c(2, 0, 0, 2 * sqrt_1_3))
    expect_equal(bar$y, c(0, 2, 0, 2 * sqrt_1_3))
    expect_equal(bar$z, c(0, 0, 2, 2 * sqrt_1_3))

    expect_equal(baz$x, c(3, 0, 0, 3 * sqrt_1_3))
    expect_equal(baz$y, c(0, 3, 0, 3 * sqrt_1_3))
    expect_equal(baz$z, c(0, 0, 3, 3 * sqrt_1_3))
  }
)

test_that(
  paste(
    "Vector3 can be multiplied both left and right by numeric vector",
    "when vector3 is length 1"
  ), {

    foo <- vector3(1, -2, 3)
    bar <- foo * seq(1, 7, by = 2)
    baz <- c(1, 2, 3, 4) * foo

    expect_equal(bar$x, c(1, 3, 5, 7))
    expect_equal(bar$y, c(-2, -6, -10, -14))
    expect_equal(bar$z, c(3, 9, 15, 21))

    expect_equal(baz$x, c(1, 2, 3, 4))
    expect_equal(baz$y, c(-2, -4, -6, -8))
    expect_equal(baz$z, c(3, 6, 9, 12))
  }
)

test_that(
  paste(
    "Vector3 can be multiplied both left and right by numeric vector",
    "the same length as the vector3 vector"
  ), {
    fac <- seq(1, 7, by = 2)
    bar <- foo * c(1, 2, 3, 4)
    baz <- fac * foo

    expect_equal(bar$x, c(1, 0, 0, 4 * sqrt_1_3))
    expect_equal(bar$y, c(0, 2, 0, 4 * sqrt_1_3))
    expect_equal(bar$z, c(0, 0, 3, 4 * sqrt_1_3))

    expect_equal(baz$x, c(1, 0, 0, 7 * sqrt_1_3))
    expect_equal(baz$y, c(0, 3, 0, 7 * sqrt_1_3))
    expect_equal(baz$z, c(0, 0, 5, 7 * sqrt_1_3))
  }
)

test_that("Vector3 magnitudes are correctly computed", {
  distances <- vector3(
    x = c(-.5, .5, sqrt(2) / 2, 3),
    y = c(-.5, -sqrt(2) / 2, .5, 4),
    z = c(sqrt(2) / 2, .5, -.5, 0)
  )

  expect_equal(magnitude(distances), c(1, 1, 1, 5))
})

test_that("Vector3 distances are correctly computed", {
  froms <- vector3(
    x = c(1, -4, 4, 3),
    y = c(0, -3, 5, 4),
    z = c(-2, 5, -7, 0)
  )

  expect_equal(
    distance_between(froms, to = vector3(0, 2, -3)),
    c(sqrt(6), sqrt(105), sqrt(41), sqrt(22))
  )
})

test_that("Vector3 can be normalized", {
  normable <- vector3(
    x = c(1, 0, 4, 3),
    y = c(0, 0, 5, 4),
    z = c(-1, 5, -7, 0)
  )
  unit <- direction(normable)
  normalized_distance <- direction(normable) %>% magnitude()

  expect_equal(head(unit$x, 2), c(sqrt(2) / 2, 0))
  expect_equal(head(unit$y, 2), c(0, 0))
  expect_equal(head(unit$z, 2), c(-sqrt(2) / 2, 1))

  expect_equal(normalized_distance, c(1, 1, 1, 1))
})

test_that("Vector3 can have a unary negative", {
  neg <- -foo
  canceled <- foo + neg

  expect_equal(neg$x, -foo_px)
  expect_equal(neg$y, -foo_py)
  expect_equal(neg$z, -foo_pz)
  expect_equal(canceled, rep(vector3(0, 0, 0), 4))
})

test_that("Vector3 can have sums, cumsums, and means", {
  sum_test_data <- vector3(
    x = c(1, 0, -1, 5),
    y = c(4, -2, 0, 1),
    z = c(-1, 2, 4, 3)
  )
  sum_foo <- sum(sum_test_data)
  cumsum_foo <- cumsum(sum_test_data)
  mean_foo <- mean(sum_test_data)

  expect_equal(sum_foo, vector3(5, 3, 8))

  expect_equal(cumsum_foo$x, c(1, 1, 0, 5))
  expect_equal(cumsum_foo$y, c(4, 2, 2, 3))
  expect_equal(cumsum_foo$z, c(-1, 1, 5, 8))

  expect_equal(mean_foo, vector3(5 / 4, 3 / 4, 8 / 4))
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
  expect_equal(dot(foo, foo), magnitude(foo)^2)

  # but fails when dotting with length-3 numerics
  expect_error(dot(foo, c(1, 0, 0)), class = "dddr_error_math")
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

  # but fails when crossed with length-3 numerics
  expect_error(cross(foo, c(1, 0, 0)), class = "dddr_error_math")

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
    class = "vctrs_error_incompatible_op",
    regexp = "vector3_prod"
  )
)

test_that("format and print work sensibly", {
  print_test <- vector3(
    x = cos(seq(0, 4 * pi, by = 0.5)),
    y = sin(seq(0, 4 * pi, by = 0.5)),
    z = exp(seq(0, 4 * pi, by = 0.5))
  )

  expect_snapshot_output(
    print(print_test)
  )

  expect_error(
    pillar::pillar_shaft(print_test) %>% format(width = 26),
    class = "simpleError"
  )

  expect_snapshot_output(
    pillar::pillar_shaft(print_test) %>% format(width = 27) %>% crayon::strip_style() %>% print
  )
  expect_snapshot_output(
    pillar::pillar_shaft(print_test) %>% format(width = 28) %>% crayon::strip_style() %>% print
  )
  expect_snapshot_output(
    pillar::pillar_shaft(print_test) %>% format(width = 29) %>% crayon::strip_style() %>% print
  )
  expect_snapshot_output(
    pillar::pillar_shaft(print_test) %>% format(width = 30) %>% crayon::strip_style() %>% print
  )
  expect_snapshot_output(
    pillar::pillar_shaft(print_test) %>% format(width = 35) %>% crayon::strip_style() %>% print
  )
})

test_that("Errors occur in bad arith types", {
  op_error_class <- "vctrs_error_incompatible_op"
  # note: it is possible for "vector3_arith" to come out of sync
  # with the real name of the documentation.
  redirection <- "vector3_arith"

  # error in default vector3 operation
  expect_error(
    simple_vector + "foobar",
    class = op_error_class,
    regexp = redirection
  )

  # incompatible vector and vector operation
  expect_error(
    simple_vector / simple_vector,
    class = op_error_class,
    regexp = redirection
  )

  # vector / numeric legnth error
  expect_error(
    simple_vector * c(0, 5),
    class = op_error_class,
    regexp = redirection
  )

  # unimplemented vector and numeric operation
  expect_error(
    simple_vector ^ c(0, 5),
    class = op_error_class,
    regexp = redirection
  )

  # unimplemented vector and missing operation
  expect_error(
    !simple_vector,
    class = op_error_class,
    regexp = redirection
  )

  # unimplemented nuermic and vector operation
  expect_error(
    4 / simple_vector,
    class = op_error_class,
    regexp = redirection
  )
})

test_that("Errors occur in bad math types", {
  op_error_class <- "dddr_error_math"
  redirection <- "vector3_math"

  expect_error(
    exp(simple_vector),
    class = op_error_class,
    regex = redirection
  )

})

test_that("is.na computes correctly", {
  expect_equal(
    is.na(vector3(
      c(3, 4, NA, 5),
      c(NA, 4, 0, 0),
      c(NA, 3, 1, 5)
    )),
    c(T, F, T, F)
  )
})

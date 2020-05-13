context("everything")
library(tidyverse)
library(vrmvrm)

simple_axes_tbl <- tribble(
  ~foo_px, ~foo_py, ~foo_pz,
        1,       0,       0,
        0,       1,       0,
        0,       0,       1
) %>% as_vrm_df()

doubled <- tribble(
  ~foo_px, ~foo_py, ~foo_pz, ~bar_px, ~bar_py, ~bar_pz,
        1,       0,       0,       2,       0,       0,
        0,       1,       0,       0,       2,       0,
        0,       0,       1,       0,       0,       2
) %>%
  as_vrm_df %>%
  collate(
    foo = list(px=foo_px, py=foo_py, pz=foo_pz)
  )


test_that("addition", {
  expect_equal(
    doubled,
    simple_axes_tbl %>%
      collate(foo = list(px=foo_px, py=foo_py, pz=foo_pz)) %>%
      mutate(bar = foo + foo)
  )
})

test_that("vector3 call", {
  expect_equal(
    doubled,
    simple_axes_tbl %>%
      mutate(
        foo = vector3(px=foo_px, py=foo_py, pz=foo_pz),
        bar = foo + foo
      )
  )
})

test_that("vector3 call, two separate mutates", {
  expect_equal(
    doubled,
    simple_axes_tbl %>%
      mutate(foo = vector3(px=foo_px, py=foo_py, pz=foo_pz)) %>%
      mutate(bar = foo + foo)
  )
})

tripled <- tribble(
  ~foo_px, ~foo_py, ~foo_pz, ~bar_px, ~bar_py, ~bar_pz, ~baz_px, ~baz_py, ~baz_pz,
  1,       0,       0,       2,       0,       0,       3,       0,       0,
  0,       1,       0,       0,       2,       0,       0,       3,       0,
  0,       0,       1,       0,       0,       2,       0,       0,       3
) %>%
  as_vrm_df %>%
  collate(
    foo = list(px=foo_px, py=foo_py, pz=foo_pz)
  )

test_that("cascading additions", {
  expect_equal(
    tripled,
    simple_axes_tbl %>%
      collate(foo = list(px=foo_px, py=foo_py, pz=foo_pz)) %>%
      mutate(
        bar = foo + foo,
        baz = bar + foo
      )
  )
})

test_that("cascading additions, two mutates", {
  expect_equal(
    tripled,
    simple_axes_tbl %>%
      collate(foo = list(px=foo_px, py=foo_py, pz=foo_pz)) %>%
      mutate(bar = foo + foo) %>%
      mutate(baz = bar + foo)
  )
})

modified <- tribble(
  ~foo_px, ~foo_py, ~foo_pz, ~bar_px, ~bar_py, ~bar_pz,
  1,       0,       1,       2,       0,       2,
  0,       1,       1,       0,       2,       2,
  0,       0,       1,       0,       0,       2
) %>%
  as_vrm_df %>%
  collate(
    foo = list(px=foo_px, py=foo_py, pz=foo_pz)
  )

test_that("calls use the most recent version of the values", {
  expect_equal(
    modified,
    simple_axes_tbl %>%
      collate(foo = list(px=foo_px, py=foo_py, pz=foo_pz)) %>%
      mutate(
        foo_pz = 1,
        bar = foo + foo
      )
  )
})

test_that("calls use the most recent version of the values", {
  expect_equal(
    modified,
    simple_axes_tbl %>%
      collate(foo = list(px=foo_px, py=foo_py, pz=foo_pz)) %>%
      mutate(foo_pz = 1) %>%
      mutate(bar = foo + foo)
  )
})







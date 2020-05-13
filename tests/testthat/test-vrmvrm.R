context("everything")
library(tidyverse)
library(vrmvrm)

simple_axes_tbl <- tribble(
  ~foo_px, ~foo_py, ~foo_pz,
        1,       0,       0,
        0,       1,       0,
        0,       0,       1
) %>% as_vrm_df %>%
  collate(foo = list(px=foo_px, py=foo_py, pz=foo_pz))

doubled <- tribble(
  ~foo_px, ~foo_py, ~foo_pz, ~bar_px, ~bar_py, ~bar_pz,
        1,       0,       0,       2,       0,       0,
        0,       1,       0,       0,       2,       0,
        0,       0,       1,       0,       0,       2
) %>% as_vrm_df %>%
  collate(
    foo = list(px=foo_px, py=foo_py, pz=foo_pz)
  )


test_that("vectors can be added", {
  expect_equal(
    doubled, simple_axes_tbl %>% mutate(bar = foo + foo)
  )
})

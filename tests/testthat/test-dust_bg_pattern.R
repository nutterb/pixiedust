context("dust_bg_pattern")

test_that("dust_bg_pattern: cast error when across is neither row nor col",
{
  expect_error(dust_bg_pattern(across = "diagonal"),
               "1: 'across' should be one of ")
})
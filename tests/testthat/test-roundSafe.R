context("roundSafe")

test_that("roundSafe: skip rounding for characters",
{
  x <- c(1.765, "hello", 8.3985, "world")
  expect_equal(roundSafe(x, 2),
               c("1.76", "hello", "8.4", "world"))
})
  
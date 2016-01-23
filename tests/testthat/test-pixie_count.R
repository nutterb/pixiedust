context("pixie_count")

test_that("get_pixie_count",
{
  expect_equal(get_pixie_count(), 0)
})

test_that("set_pixie_count",
{
  set_pixie_count(4)
  expect_equal(get_pixie_count(), 4)  
})

test_that("increment_pixie_count",
{
  set_pixie_count(0)
  increment_pixie_count(1)
  expect_equal(get_pixie_count(), 1)
})

test_that("increment_pixie_count, non-1 increment",
{
  set_pixie_count(3)
  increment_pixie_count(2)
  expect_equal(get_pixie_count(), 5)
})
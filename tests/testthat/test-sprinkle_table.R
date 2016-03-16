context("sprinkle_table")

test_that("sprinkle_table: apply a sprinkle to all parts",
{
  x <- dust(mtcars)
  expect_silent(sprinkle_table(x, round = 2))
})

test_that("sprinkle_table: no valid part names",
{
  x <- dust(mtcars)
  expect_error(sprinkle_table(x, round = 2, part = "that part yonder"))
})
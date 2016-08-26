context("sprinkle_logical_row")

test_that("sprinkle: select rows using the logical_row argument",
{
  x <- dust(mtcars)
  expect_silent(sprinkle(x, logical_rows = quote(mpg > 20), bg = "lightblue"))
})


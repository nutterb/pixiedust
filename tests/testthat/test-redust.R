context("redust")

test_that("redust: reject tables with dissimilar column dimension",
{
  x <- dust(mtcars[1:10, ])
  expect_error(redust(x, mtcars[1:2, 1:9], part = "head"),
               "The current table has 11 columns")
})
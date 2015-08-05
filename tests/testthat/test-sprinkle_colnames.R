context("sprinkle_colnames")

test_that("sprinkle_colnames produces output",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_that(sprinkle_colnames(x, "Term", "Estimate", "SE", "T-stat", "Pval"),
              not(throws_error()))
})

test_that("sprinkle_colnames errors",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle_colnames(x, estimate = "Estimate", "SE"),
               "1: Elements of '...' must either all be named or all be unnamed")
})

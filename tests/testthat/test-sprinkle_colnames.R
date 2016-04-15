context("sprinkle_colnames")

test_that("sprinkle_colnames produces output",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_silent(sprinkle_colnames(x, "Term", "Estimate", "SE", "T-stat", "Pval"))
})

test_that("sprinkle_colnames errors",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle_colnames(x, estimate = "Estimate", "SE"),
               "")
})

test_that("sprinkle_colnames: unnamed arguments have length 1",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle_colnames(x, "Term", "Estimate", "SE", "Statistic", c("P.value", "P")),
               "Arguments to '...' should have length 1")
})

test_that("sprinkle_colnames: named argument have lenght 1",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle_colnames(x, estimate = c("Estimate", "est")),
               "Arguments to '...' should have length 1")
})

test_that("sprinkle_colnames: named arguments",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_silent(sprinkle_colnames(x, estimate = "Estimate", std.error = "Std. Error"))
})

test_that("sprinkle_colnames: cast error when named argument doesn't match to table",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle_colnames(x, estimate_bad = "Estimate", std.error = "Std. Error"))
})
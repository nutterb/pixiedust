context("sprinkle_colnames")

test_that("sprinkle_colnames produces output",
{
  expect_that(sprinkle_colnames("Estimate", "SE", "Pval"),
              not(throws_error()))
})

test_that("sprinkle_colnames errors",
{
  expect_error(sprinkle_colnames(estimate = "Estimate", "SE"),
               "1: Elements of '...' must either all be named or all be unnamed")
})

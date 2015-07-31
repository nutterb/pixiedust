context("pvalString")

test_that("pvalString",
{
  expect_equal(pvalString(c(1, .9, .5, .111, .101, .0994, .0515, .0486, .001, .0001)),
               c('> 0.99', '0.90', '0.50', '0.11', '0.10', '0.099', '0.052', 
                 '0.049', '< 0.001', '< 0.001'))
})

test_that("pvalString errors",
{
  expect_error(pvalString(1.3),
               "Element[(]s[)] 1 are not valid probabilities")
})
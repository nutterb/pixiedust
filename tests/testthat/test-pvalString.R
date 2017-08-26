context("pvalString")

test_that("pvalString",
{
  expect_equal(pvalString(c(1, .9, .5, .111, .101, .0994, .0515, .0486, .001, .0001)),
               c('> 0.99', '0.90', '0.50', '0.11', '0.10', '0.099', '0.052', 
                 '0.049', '< 0.001', '< 0.001'))
})

test_that("pvalString errors",
{
  expect_error(pvalString(1.3))
})

test_that(
  "pvalString exact",
  {
    expect_equal(pvalString(c(1, .9, .5, .111, .101, .0994, .0515, .0486, .001, .0001),
                            format = "exact"),
                 c('1.000', '0.900', '0.500', '0.111', '0.101', '0.099', '0.052', 
                   '0.049', '0.001', '1.00e-04'))
  }
)

test_that(
  "pvalString scientific",
  {
    expect_equal(pvalString(c(1, .9, .5, .111, .101, .0994, .0515, .0486, .001, .0001),
                            format = "scientific"),
                 c('1.00e+00', '9.00e-01', '5.00e-01', '1.11e-01', '1.01e-01', 
                   '9.94e-02', '5.15e-02', '4.86e-02', '1.00e-03', '1.00e-04'))
  }
)
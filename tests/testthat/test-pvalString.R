context("pval_string")

# Functional Requirement 1 ------------------------------------------

test_that(
  "pval_string default",
  {
    expect_equal(pval_string(c(1, .9, .5, .111, .101, .0994, .0515, .0486, .001, .0001)),
                 c('> 0.99', '0.90', '0.50', '0.11', '0.10', '0.099', '0.052', 
                   '0.049', '0.001', '< 0.001'))
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "pval_string exact",
  {
    expect_equal(pval_string(c(1, .9, .5, .111, .101, .0994, .0515, .0486, .001, .0001),
                            format = "exact"),
                 c('1.000', '0.900', '0.500', '0.111', '0.101', '0.099', '0.052', 
                   '0.049', '0.001', '1.00e-04'))
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "pval_string scientific",
  {
    expect_equal(pval_string(c(1, .9, .5, .111, .101, .0994, .0515, .0486, .001, .0001),
                            format = "scientific"),
                 c('1.00e+00', '9.00e-01', '5.00e-01', '1.11e-01', '1.01e-01', 
                   '9.94e-02', '5.15e-02', '4.86e-02', '1.00e-03', '1.00e-04'))
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "cast an error if p is not numeric on the interval [0, 1]",
  {
    expect_error(pval_string(1.3))
  }
)

test_that(
  "Cast an error if p is not numeric on the interval [0, 1]",
  {
    expect_error(pval_string(letters))
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "Cast an error if format is not one of default, exact, or scientific",
  {
    expect_error(pval_string(0.3, format = "not a format"))
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "Cast an error if digits is not integerish(1)",
  {
    expect_error(pval_string(0.3, format = 'exact', digits = "three"))
  }
)

test_that(
  "Cast an error if digits is not integerish(1)",
  {
    expect_error(pval_string(0.3, format = 'exact', digits = 3:4))
  }
)
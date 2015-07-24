context("dust_colnames")

test_that("dust_colnames with no 'col_names' arguemnt",
{
  expect_equal(dust_colnames(col_names = c("Term", "Estimate", "SE", "T", "p")),
               structure(c("Term", "Estimate", "SE", "T", "p"),
                         class = c("col_names", "dust_bunny")))
})

test_that("dust_colnames returns error when named arguments have length greater than 1",
{
  expect_error(dust_colnames(term = c("Term", "SE"), std.error = "SE", p.value = "p"),
               "Arguments to 'dust_colnames' should have length 1 [(]unless using 'col_names'[)]")
})

test_that("dust_colnames returns a vector when all arguments are named",
{
  expect_equal(dust_colnames(term = "Term", std.error = "SE", p.value = "p"),
               structure(c(term = "Term", std.error = "SE", p.value = "p"),
                         class = c("col_names", "dust_bunny")))
})

test_that("dust_colnames returns an error when some but not all arguments are named",
{
  expect_error(dust_colnames(term = "Term", "SE", "p"),
               "arguments to [.][.][.] must either all be named or all be unnamed")
})

test_that("dust_colnames returns a vector when no arguments are named",
{
  expect_equal(dust_colnames("Term", "SE", "p"),
               structure(c("Term", "SE", "p"),
                         class = c("col_names", "dust_bunny")))
})

test_that("dust_colnames: named arguments are added to dust table",
{
  x <- dust(lm(mpg ~ qsec + factor(am), data = mtcars)) + 
    dust_colnames(term = "Term", statistic = "T")
  expect_equal(x$col_names, 
               c(term = "Term", estimate = "estimate", 
                 std.error = "std.error", statistic = "T", 
                 p.value = "p.value"))
})

test_that("dust_colnames: unnamed arguments are added to dust table",
{
  x <- dust(lm(mpg ~ qsec + factor(am), data = mtcars)) + 
    dust_colnames("Term", "Estimate", "SE", "T-statistic", "p-value")
  expect_equal(x$col_names,
               c(term = "Term", estimate = "Estimate", 
                 std.error = "SE", statistic = "T-statistic", 
                 p.value = "p-value"))
})

test_that("dust_colnames: return error when there are not enough unnamed arguments",
{
  x <- dust(lm(mpg ~ qsec + factor(am), data = mtcars))
  expect_error(x + dust_colnames("Term", "Estimate"),
               "Unnamed column names from 'dust_colnames' should have length 5.")
})

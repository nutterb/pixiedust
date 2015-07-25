context("dust_fn")

test_that("Only one function may be passed",
{
  expect_error(dust_fn(col = 2, fn = c(quote(pvalString(value)),
                                       quote(format(value, digits=3))),
               paste0("1: 'fn' must have length 1")))
})

test_that("Correct return format",
{
  expect_equal(dust_fn(col = 2, row = 1:3, fn = quote(pvalString(value))),
               structure(list(col = 2, row = 1:3, fn = quote(pvalString(value))), 
                         .Names = c("col", "row", "fn"), 
                         class = c("dust_fn", "dust_bunny")))
})

test_that("Check another correct return format",
{
  expect_equal(dust_fn(col = 2, row = 1:3, colname = c("statistic", "p.value"), 
                       fn = quote(format(value, digits = 3))),
               structure(list(col = 2, row = 1:3, 
                              colname = c("statistic", "p.value"), 
                              fn = quote(format(value, digits = 3))), 
                         .Names = c("col", "row", "colname", "fn"), 
                         class = c("dust_fn", "dust_bunny")))
})


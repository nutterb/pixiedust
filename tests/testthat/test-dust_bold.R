context("dust_bold")

test_that("dust_bold: set_bold must have length 1",
{
  expect_error(dust_bold(col = 2, set_bold = c(TRUE, FALSE)),
               paste0("1: 'set_bold' must have length 1"))
})

test_that("dust_bold: set_bold must be logical",
{
  expect_error(dust_bold(col = 2, set_bold = "yes please"),
               paste0("1: 'set_bold' must be logical"))
})

test_that("dust_bold: Correct return format",
{
  expect_equal(dust_bold(col = 2, row = 1:3, set_bold = TRUE),
               structure(list(col = 2, row = 1:3, set_bold = TRUE), 
                         .Names = c("col", "row", "set_bold"), 
                         class = c("dust_bold", "dust_bunny")))
})

test_that("dust_bold: Check another correct return format",
{
  expect_equal(dust_bold(col = 2, row = 1:3, colname = c("statistic", "p.value"), set_bold = FALSE),
               structure(list(col = 2, row = 1:3, 
                              colname = c("statistic", "p.value"), 
                              set_bold = FALSE), 
                         .Names = c("col", "row", "colname", "set_bold"), 
                         class = c("dust_bold", "dust_bunny")))
})


context("dust_italic")

test_that("set_italic must have length 1",
{
  expect_error(dust_italic(col = 2, set_italic = c(TRUE, FALSE)),
               paste0("1: 'set_italic' must have length 1"))
})

test_that("set_italic must be logical",
{
  expect_error(dust_italic(col = 2, set_italic = "yes please"),
               paste0("1: 'set_italic' must be logical"))
})

test_that("Correct return format",
{
  expect_equal(dust_italic(col = 2, row = 1:3, set_italic = TRUE),
               structure(list(col = 2, row = 1:3, set_italic = TRUE), 
                         .Names = c("col", "row", "set_italic"), 
                         class = c("dust_italic", "dust_bunny")))
})

test_that("Check another correct return format",
{
  expect_equal(dust_italic(col = 2, row = 1:3, colname = c("statistic", "p.value"), set_italic = FALSE),
               structure(list(col = 2, row = 1:3, 
                              colname = c("statistic", "p.value"), 
                              set_italic = FALSE), 
                         .Names = c("col", "row", "colname", "set_italic"), 
                         class = c("dust_italic", "dust_bunny")))
})


context("dust_cell_bg")

test_that("dust_cell_bg: color must have length 1",
{
  expect_error(dust_cell_bg(col = 2, color = c("red", "blue")),
               paste0("1: 'color' must have length 1"))
})

test_that("dust_cell_bg: color must be logical",
{
  expect_error(dust_cell_bg(col = 2, color = TRUE),
               paste0("1: 'color' must be a character value"))
})

test_that("dust_cell_bg: Correct return format",
{
  expect_equal(dust_cell_bg(col = 2, row = 1:3, color = "gray"),
               structure(list(col = 2, row = 1:3, color = "gray"), 
                         .Names = c("col", "row", "color"), 
                         class = c("dust_cell_bg", "dust_bunny")))
})

test_that("dust_cell_bg: Check another correct return format",
{
  expect_equal(dust_cell_bg(col = 2, row = 1:3, colname = c("statistic", "p.value"), color = "gray"),
               structure(list(col = 2, row = 1:3, 
                              colname = c("statistic", "p.value"), 
                              color = "gray"), 
                         .Names = c("col", "row", "colname", "color"), 
                         class = c("dust_cell_bg", "dust_bunny")))
})


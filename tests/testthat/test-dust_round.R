context("dust_round")

test_that("dust_round: round must have length 1",
{
  expect_error(dust_round(col = 2, round = c(2, 3)),
               paste0("1: 'round' must have length 1"))
})

test_that("dust_round: set_round must be numeric",
{
  expect_error(dust_round(col = 2, round = "yes please"),
               paste0("1: 'round' argument in 'dust_round' must be numeric"))
})

test_that("dust_round: Correct return format",
{
  expect_equal(dust_round(col = 2, row = 1:3, round = 3),
               structure(list(col = 2, row = 1:3, round = 3), 
                         .Names = c("col", "row", "round"), 
                         class = c("dust_round", "dust_bunny")))
})

test_that("dust_round: Check another correct return format",
{
  expect_equal(dust_round(col = 2, row = 1:3, colname = c("statistic", "p.value"), round = 3),
               structure(list(col = 2, row = 1:3, 
                              colname = c("statistic", "p.value"), 
                              round = 3), 
                         .Names = c("col", "row", "colname", "round"), 
                         class = c("dust_round", "dust_bunny")))
})


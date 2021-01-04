test_that("ungroup a grouped_data",
{
  expect_silent(
    dust(mtcars %>%
          poorman::group_by(am, vs))
  )
})

test_that("split grouped_data",
{
  expect_silent(
    dust(mtcars %>%
           poorman::group_by(am, vs),
         ungroup = FALSE)
  )
})
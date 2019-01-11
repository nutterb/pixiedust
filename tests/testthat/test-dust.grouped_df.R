context("Dust a grouped_df")

test_that("ungroup a grouped_df",
{
  expect_silent(
    dust(mtcars %>%
                 dplyr::group_by(am, vs))
  )
})

test_that("split a grouped_df",
{
  expect_silent(
    dust(mtcars %>%
           dplyr::group_by(am, vs),
         ungroup = FALSE)
  )
})

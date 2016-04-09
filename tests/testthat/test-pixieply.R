context("pixieply")

test_that("apply medley to dust_list",
{
  expect_silent(
    pixieply(
      dust(mtcars %>%
             dplyr::group_by(am, vs),
           ungroup = FALSE),
      medley_all_borders
    )
  )
})
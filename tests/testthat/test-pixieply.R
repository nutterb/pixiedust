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


test_that(
  "apply unique captions to dust_list",
  {
    mtcars %>%
      dplyr::group_by(gear) %>%
      dust(ungroup = FALSE) %>%
      pixiemap(FUN = sprinkle,
               caption = sprintf("gear = %s", sort(unique(mtcars$gear)))) %>%
      expect_silent()
  }
)

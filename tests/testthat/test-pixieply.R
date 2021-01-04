test_that("apply medley to dust_list",
{
  expect_silent(
    pixieply(
      dust(mtcars %>%
             # TODO: waiting on poorman release of .drop in group_by
             poorman::group_by(am, vs),
           ungroup = FALSE),
      medley_all_borders
    )
  )
})

test_that(
  "apply unique captions to dust_list",
  {
    mtcars %>%
      # TODO: waiting on poorman release of .drop in group_by
      poorman::group_by(gear) %>%
      dust(ungroup = FALSE) %>%
      pixiemap(FUN = sprinkle,
               caption = sprintf("gear = %s", sort(unique(mtcars$gear)))) %>%
      expect_silent()
  }
)

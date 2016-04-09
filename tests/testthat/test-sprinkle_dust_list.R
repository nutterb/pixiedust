context("sprinkling dust_list")

dlist <- dust(mtcars %>%
                dplyr::group_by(am, vs),
              ungroup = FALSE)

test_that("sprinkle_table on a dust_list",
{
  expect_silent(
    sprinkle_table(dlist, longtable = TRUE)
  )
})

test_that("sprinkle_print_method on a dust_list",
{
  expect_silent(
    sprinkle_print_method(dlist, "html")
  )
})

test_that("sprinkle_colnames on a dust_list",
{
  expect_silent(
    sprinkle_colnames(dlist, mpg = "MPG", cyl = "CYL")
  )
})

test_that("sprinkle a dust_list",
{
  expect_silent(
    sprinkle(dlist, 
             rows = 3, 
             cols = 2,
             italic = TRUE)
  )
})

test_that("convert dust_list to list of data.frames",
{
  expect_silent(
    as.data.frame(dlist)
  )
})
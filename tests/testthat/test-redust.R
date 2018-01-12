context("redust")

test_that("redust: reject tables with dissimilar column dimension",
{
  x <- dust(mtcars[1:10, ])
  expect_error(redust(x, mtcars[1:2, 1:9], part = "head"),
               "The current table has 11 columns")
})

test_that("redust: catch mismatched column counts, perhaps from someone adding a table without redust",
{
  x <- dust(mtcars[1:10, ])
  x$foot <- dust(mtcars[1:2, 1:9])$body
  expect_error(redust(x, mtcars[1:2, 1:11], part = "head"))
})

test_that("redust: how about a working example",
{
  x <- dust(mtcars[1:10, ])
  expect_silent(redust(x, mtcars[1:2, ], part = "head"))
})

test_that(
  "redust: dust_list",
  {
    expect_silent(
      dplyr::group_by(mtcars, am, vs) %>% 
        dust(ungroup = FALSE) %>% 
        redust(mtcars[1:2, ], part = "head")
    )
  }
)
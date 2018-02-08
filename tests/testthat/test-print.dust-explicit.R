context("print.dust-explicit")

test_that("print.dust for console output",
{
  expect_output(
    print(dust(mtcars) %>% sprinkle_print_method("console"))
  )
})

test_that("print.dust for markdown output",
{
  expect_silent(
    print(dust(mtcars) %>% sprinkle_print_method("markdown"))
  )
})

test_that("print.dust for html output",
{
  expect_silent(
    print(dust(mtcars) %>% sprinkle_print_method("html"))
  )
})

test_that("print.dust for latex output with hhline = FALSE",
{
  expect_silent(
    print(dust(mtcars) %>% sprinkle_print_method("latex"))
  )
})

test_that("print.dust for latex output with hhline = TRUE",
{
  expect_silent(
    print(dust(mtcars, hhline = TRUE) %>% sprinkle_print_method("latex"))
  )
})

test_that("print.dust for unsupported format",
{
  expect_error(
    print(dust(mtcars) %>% sprinkle_print_method("unsupported format"))
  )
})

test_that("print.dust_list",
{
  x <- split(mtcars, list(mtcars$am, mtcars$vs))
  expect_output(
    print(dust(x))
  )
})

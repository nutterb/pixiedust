context("print.dust")

test_that("print.dust for console output",
{
  expect_output(
    print(dust(mtcars) %>% sprinkle_print_method("console")),
    regexp = ""
  )
})

test_that("print.dust for markdown output",
{
  expect_output(
    print(dust(mtcars) %>% sprinkle_print_method("markdown")),
    regexp = ""
  )
})

test_that("print.dust for html output",
{
  expect_output(
    print(dust(mtcars) %>% sprinkle_print_method("html")),
    regexp = ""
  )
})

test_that("print.dust for latex output with hhline = FALSE",
{
  expect_output(
    print(dust(mtcars) %>% sprinkle_print_method("latex")),
    regexp = ""
  )
})

test_that("print.dust for latex output with hhline = TRUE",
{
  expect_output(
    print(dust(mtcars, hhline = TRUE) %>% sprinkle_print_method("latex")),
    regexp = ""
  )
})

test_that("print.dust for unsupported format",
{
  expect_error(
    print(dust(mtcars) %>% sprinkle_print_method("unsupported format"))
  )
})

test_that("prind.dust_list",
{
  x <- split(mtcars, list(mtcars$am, mtcars$vs))
  expect_output(
    print(dust(x)),
    regexp = ""
  )
})

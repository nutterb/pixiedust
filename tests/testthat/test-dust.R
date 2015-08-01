context("Create a dust object")

test_that("Create a dust object",
{
  fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
  x <- dust(fit)
  
  expect_equal(class(x), "dust")
})

test_that("dust object has expected names",
{
  fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
  x <- dust(fit)
  
  expect_equal(names(x), c("head", "body", "interfoot", "foot", "table_attributes",
                           "border_collapse", "object", "print_method"))
})

test_that("dust object body component has correct dimensions",
{
  fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
  x <- dust(fit)
  
  Dims <- list(dim(x$head),
               dim(x$body),
               dim(x$interfoot),
               dim(x$foot))
  
  expect_equal(Dims, 
               list(c(5, 25), 
                    c(30, 25),
                    NULL,
                    NULL))
})

test_that("dust runs when passed a data frame with tidy_df = FALSE",
{
  expect_output(dust(mtcars, tidy_df = FALSE), "mpg")
})

test_that("dust runs when passed a data frame with tidy_df = TRUE",
{
  expect_output(dust(mtcars, tidy_df = TRUE), "column")
})
context("Create a dust object")

fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)

test_that("Create a dust object",
{
  x <- dust(fit)
  
  expect_equal(class(x), "dust")
})

test_that("dust object has expected names",
{
  x <- dust(fit)
  
  expect_equal(names(x), c("head", "body", "interfoot", "foot", 
                           "border_collapse", "caption", "float", 
                           "longtable", "table_width", "tabcolsep", 
                           "print_method"))
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
               list(c(5, 32), 
                    c(30, 32),
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

test_that("dust with keep_rownames = TRUE adds rownames to object",
{
  x <- dust(mtcars, keep_rownames = TRUE)
  expect_equal(x$body$value[1:32], rownames(mtcars))
})

test_that("dust with additional descriptors",
{
  expect_that(dust(fit, 
                    descriptors = c("label", "level_detail")),
               not(throws_error()))
})

test_that("dust with additional descriptors and term_plain numeric_label",
{
  expect_that(dust(fit,
                   descriptors = c("label", "level_detail"),
                   numeric_label = "term_plain"),
              not(throws_error()))
})

test_that("dust with glance_foot",
{
  expect_that(dust(fit, glance_foot = TRUE),
              not(throws_error()))
})

test_that("dust with glance_foot and col_pairs a divisor of total_cols",
{
  fit <- lm(mpg ~ qsec + factor(am) + wt * factor(gear), data = mtcars)
  expect_that(dust(fit,
                   descriptors = c("label", "level_detail"),
                   glance_foot = TRUE, col_pairs = 3),
              not(throws_error()))
})
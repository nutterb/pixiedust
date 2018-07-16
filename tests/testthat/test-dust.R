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
  
  expect_equal(names(x), 
               c("head",            "body",        "interfoot",   "foot", 
                 "border_collapse", "caption",     "caption_number", "label",       
                 "justify", 
                 "float",           "longtable",   "table_width", "tabcolsep", 
                 "hhline",          "bookdown",    "fixed_header", "include_fixed_header_css",
                 "fixed_header_param", 
                 "html_preserve",   "print_method"))
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
               list(c(5, 37), 
                    c(30, 37),
                    NULL,
                    NULL))
})

test_that("dust runs when passed a data frame with tidy_df = FALSE",
{
  expect_silent(dust(mtcars, tidy_df = FALSE))
})

test_that("dust runs when passed a data frame with tidy_df = TRUE",
{
  # 25 Jun 2018 Changed to expect warning since broom is deprecating data frame
  #             tidiers
  if (utils::compareVersion(as.character(packageVersion("broom")), "0.4.5") == 1)
    expect_warning(dust(mtcars, tidy_df = TRUE))
  else
    expect_silent(dust(mtcars, tidy_df = TRUE))
})

test_that("dust with keep_rownames = TRUE adds rownames to object",
{
  x <- dust(mtcars, keep_rownames = TRUE)
  expect_equal(x$body$value[1:32], rownames(mtcars))
})

test_that("dust with additional descriptors",
{
  expect_silent(dust(fit, 
                    descriptors = c("label", "level_detail")))
})

test_that("dust with additional descriptors and term_plain numeric_label",
{
  expect_silent(dust(fit,
                   descriptors = c("label", "level_detail"),
                   numeric_label = "term_plain"))
})

test_that("dust with glance_foot",
{
  expect_silent(dust(fit, glance_foot = TRUE))
})

test_that("dust with glance_foot and col_pairs a divisor of total_cols",
{
  fit <- lm(mpg ~ qsec + factor(am) + wt * factor(gear), data = mtcars)
  expect_silent(dust(fit,
                     descriptors = c("label", "level_detail"),
                     glance_foot = TRUE, 
                     col_pairs = 3))
})


test_that("dust a list",
{
  x <- split(mtcars, list(mtcars$am, mtcars$vs))
  expect_silent(dust(x))
})


test_that(
  "dust with descriptors",
  {
    expect_silent(
      dust(fit, descriptors = c("term_plain", "label", "level"))
    )
  }
)

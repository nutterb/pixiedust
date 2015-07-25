context("dust_head_halign")

test_that("dust_head_halign with no 'col_names' arguemnt",
{
  expect_equal(dust_head_halign(attr_vec = c("r", "l", "left", "center", "c")),
               structure(c("r", "l", "l", "c", "c"), 
                         class = c("dust_head_halign", "dust_bunny")))
})

test_that("dust_head_halign returns error when named arguments have length greater than 1",
{
  expect_error(dust_head_halign(term = c("c", "l"), std.error = "right", p.value = "c"),
               "Arguments to 'dust_head_halign' should have length 1 [(]unless using 'attr_vec'[)]")
})

test_that("dust_head_halign returns a vector when all arguments are named",
{
  expect_equal(dust_head_halign(term = "left", std.error = "r", p.value = "center"),
               structure(c("l", "r", "c"), 
                         .Names = c("term", "std.error", "p.value"), 
                         class = c("dust_head_halign", "dust_bunny")))
})

test_that("dust_head_halign returns an error when some but not all arguments are named",
{
  expect_error(dust_head_halign(term = "left", "center", "right"),
               "arguments to [.][.][.] must either all be named or all be unnamed")
})

test_that("dust_head_halign returns a vector when no arguments are named",
{
  expect_equal(dust_colnames("left", "center", "right"),
               structure(c("left", "center", "right"), 
                         class = c("col_names", "dust_bunny")))
})

test_that("dust_head_halign: named arguments are added to dust table",
{
  x <- dust(lm(mpg ~ qsec + factor(am), data = mtcars)) + 
    dust_head_halign(term = "left", statistic = "center")
  expect_equal(x$head, 
               structure(list(col_name = c("term", "estimate", "std.error", "statistic", "p.value"), 
                              col_title = c("term", "estimate", "std.error", "statistic", "p.value"), 
                              halign = c("l", NA, NA, "c", NA), 
                              valign = c(NA, NA, NA, NA, NA), 
                              col_class = c("character", "numeric", "numeric", "numeric", "numeric")), 
                         row.names = c(NA, -5L), 
                         .Names = c("col_name", "col_title", "halign", "valign", "col_class"), 
                         class = "data.frame"))
})

test_that("dust_head_halign: unnamed arguments are added to dust table",
{
  x <- dust(lm(mpg ~ qsec + factor(am), data = mtcars)) + 
    dust_head_halign("left", "right", "right", "right", "right")
  expect_equal(x$head,
               structure(list(col_name = c("term", "estimate", "std.error", "statistic", "p.value"), 
                              col_title = c("term", "estimate", "std.error", "statistic", "p.value"), 
                              halign = c("l", "r", "r", "r", "r"), 
                              valign = c(NA, NA, NA, NA, NA), 
                              col_class = c("character", "numeric", "numeric", "numeric", "numeric")), 
                         row.names = c(NA, -5L), 
                         .Names = c("col_name", "col_title", "halign", "valign", "col_class"), 
                         class = "data.frame"))
})

test_that("dust_colnames: return error when there are not enough unnamed arguments",
{
  x <- dust(lm(mpg ~ qsec + factor(am), data = mtcars))
  expect_error(x + dust_head_halign("left", "right"),
               "Unnamed column names from 'dust_head_halign' should have length 5.")
})

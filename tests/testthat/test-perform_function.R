context("perform_function")

test_that("Apply a calculation",
{
  fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
  x <- dust(fit) %>% 
    sprinkle(row = 2, cols = 2:3, fn = quote(round(value * -1, 2)))
  
  x <- perform_function(x$body)
  
  expect_equal(x$value[x$row == 2 & x$col %in% 2:3],
               c("-1.24", "-0.38"))
})

test_that("Apply a string manipulation",
{
  fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
  x <- dust(fit) %>%
    sprinkle(cols = 1, fn = quote(gsub("factor[(]gear[)]", "Gears: ", value)))
            
  x <- perform_function(x$body)
            
  expect_equal(x$value[x$row %in% 5:6 & x$col == 1],
               c("Gears: 4", "Gears: 5"))
})
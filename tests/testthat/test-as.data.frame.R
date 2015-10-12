context("as.data.frame.dust")

test_that("as.data.frame.dust",
{
  fit <- lm(mpg ~ qsec + factor(am) + wt * factor(gear), data = mtcars)
  Dust <- dust(fit) %>%
    sprinkle(cols = 2:4, round = 2) %>%
    sprinkle(cols = 5, fn = quote(pvalString(value))) %>%
    sprinkle(cols = 3, font_color = "#DA70D6") %>%
    sprinkle_print_method("html")
  expect_true("data.frame" %in% class(as.data.frame(Dust)))
})

test_that("as.data.frame.dust unsprinkled",
{
  fit <- lm(mpg ~ qsec + factor(am) + wt * factor(gear), data = mtcars)
  Dust <- dust(fit) %>%
    sprinkle(cols = 2:4, round = 2) %>%
    sprinkle(cols = 5, fn = quote(pvalString(value))) %>%
    sprinkle(cols = 3, font_color = "#DA70D6") %>%
    sprinkle_print_method("html")
  expect_true("data.frame" %in% class(as.data.frame(Dust, sprinkled = FALSE)))
})
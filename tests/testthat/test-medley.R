context("Medleys")

fit <- lm(mpg ~ qsec + factor(am) + wt * factor(gear), data = mtcars)

test_that("medley_bw",
{
  expect_silent(
    dust(fit) %>%
      medley_bw() %>%
      sprinkle_print_method("html")
  )

})

test_that("medley_model",
{
  expect_silent(
    dust(fit, 
         glance_foot = TRUE) %>%
    medley_model() %>%
    sprinkle_print_method("html")
  )
})
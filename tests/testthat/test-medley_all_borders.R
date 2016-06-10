context("medley_all_borders")

fit <- lm(mpg ~ qsec + factor(am) + wt * factor(gear), data = mtcars)

test_that("medley_all_borders with defaults",
{
  expect_silent(
    dust(fit) %>%
      medley_all_borders()
  )
})

test_that("medley_all_borders with horizontal = FALSE",
{
  expect_silent(
    dust(fit) %>%
      medley_all_borders(rows = 2, 
                         cols = 3,
                         horizontal = FALSE)
  )
})

test_that("medley_all_borders with vertical = FALSE",
{
  expect_silent(
    dust(fit) %>%
      medley_all_borders(rows = 2:4,
                         cols = 3:5, 
                         vertical = FALSE) %>%
      sprinkle_print_method("html")
  )
})

test_that(
  "medley_all_borders with part = 'table'",
  {
    expect_silent(
      dust(fit) %>% medley_all_borders(part = "table") %>%
        sprinkle_print_method("html")
    )
  }
)
            
  
context("glance_foot")

mtcars2 <- mtcars
Hmisc::label(mtcars2$mpg) <- "Gas Mileage"
Hmisc::label(mtcars2$qsec) <- "Quarter Mile Time"
Hmisc::label(mtcars2$am) <- "Transmission"
Hmisc::label(mtcars2$wt) <- "Weight"
Hmisc::label(mtcars2$gear) <- "Gears"

fit <- lm(mpg ~ qsec + factor(am) * wt + factor(gear), data = mtcars2)

test_that("glance_foot by column",
{
  expect_silent(glance_foot(fit, col_pairs = 2, total_cols = 6))
})

test_that("glance_foot by row",
{
  expect_silent(glance_foot(fit, col_pairs = 2, total_cols = 6, byrow = TRUE))
})

test_that("glance_foot with subset of stats",
{
  expect_silent(glance_foot(fit, col_pairs = 2, total_cols = 6, byrow = TRUE,
                          glance_stats = c("r.squared", "adj.r.squared",
                                           "df", "AIC")))
})

test_that("glance_foot with invalid stats requested",
{
  expect_warning(glance_foot(fit, col_pairs = 2, total_cols = 6, byrow = TRUE,
                          glance_stats = c("r.squared", "adj.r.squared",
                                           "df", "AIC-xy")))
})

test_that("glance_foot with no valid stats requested",
{
  expect_error(
    expect_warning(
      glance_foot(
        fit, 
        col_pairs = 2, 
        total_cols = 6, 
        byrow = TRUE,
        glance_stats = c("r.squared-x", "adj.r.squared-x",
                         "df-x", "AIC-xy")
      )
    )
  )
})

test_that("glance_foot with too few total_cols",
{
  expect_error(
    expect_warning(
      glance_foot(
        fit, 
        col_pairs = 2, 
        total_cols = 3, 
        byrow = TRUE,
        glance_stats = c("r.squared", "adj.r.squared",
                         "df", "AIC-xy")
      )
    )
  )
})
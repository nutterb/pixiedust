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
  expect_that(glance_foot(fit, col_pairs = 2, total_cols = 6),
              not(throws_error()))
})

test_that("glance_foot by row",
{
  expect_that(glance_foot(fit, col_pairs = 2, total_cols = 6, byrow = TRUE),
              not(throws_error()))
})

test_that("glance_foot with subset of stats",
{
  expect_that(glance_foot(fit, col_pairs = 2, total_cols = 6, byrow = TRUE,
                          glance_stats = c("r.squared", "adj.r.squared",
                                           "df", "AIC")),
              not(throws_error()))
})

test_that("glance_foot with invalid stats requested",
{
  expect_that(glance_foot(fit, col_pairs = 2, total_cols = 6, byrow = TRUE,
                          glance_stats = c("r.squared", "adj.r.squared",
                                           "df", "AIC-xy")),
              gives_warning())
})

test_that("glance_foot with too few total_cols",
{
  expect_that(glance_foot(fit, col_pairs = 2, total_cols = 3, byrow = TRUE,
                          glance_stats = c("r.squared", "adj.r.squared",
                                           "df", "AIC-xy")),
              throws_error())
})
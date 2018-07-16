context("gaze.R")

fit1 <- lm(mpg ~ qsec + wt + factor(gear),
           data = mtcars)

fit2 <- lm(mpg ~ disp + qsec + wt + factor(gear),
           data = mtcars)

# Functional Requirement 1 ------------------------------------------

test_that(
  "Return a data frame object",
  {
    checkmate::expect_data_frame(
      gaze(fit1, fit2)
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Cast an error if include_glance is not logical(1)",
  {
    expect_error(
      gaze(fit1, fit2, include_glance = "yes")
    )
  }
)

test_that(
  "Cast an error if include_glance is not logical(1)",
  {
    expect_error(
      gaze(fit1, fit2, include_glance = c(TRUE, FALSE))
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Cast an error if glance_vars is not a character vector.",
  {
    expect_error(
      gaze(fit1, fit2, glance_vars = list(1:3, letters))
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Cast an error if digits is not integerish(1)",
  {
    expect_error(
      gaze(fit1, fit2, digits = "two")
    )
  }
)

test_that(
  "Cast an error if digits is not integerish(1)",
  {
    expect_error(
      gaze(fit1, fit2, digits = c(2, 3))
    )
  }
)
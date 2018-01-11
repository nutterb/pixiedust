context("sprinkle_fn")

x <- dust(head(mtcars))

# Functional Requirement 1 ------------------------------------------

test_that(
  "Correctly reassigns the appropriate elements of the fn column in the table part.",
  {
    expect_equal(
      sprinkle_fn(x, rows = 1, fn = quote(identity(value)))$body$fn,
      rep(c("identity(value)", rep(NA, 5)), 11)
    )
  }
)

test_that(
  "Correctly reassigns the appropriate elements bg column in the table part.",
  {
    fn <- rep(NA, nrow(x$body))
    fn[x$body$row == 2 & x$body$col %in% 4:5] <- "identity(value)"
    expect_equal(
      sprinkle_fn(x, rows = 2, cols = 4:5, fn = quote(identity(value)))$body$fn,
      fn
    )
  }
)

test_that(
  "Correctly reassigns the appropriate elements of replace column in the table part.",
  {
    fn <- rep(NA, nrow(x$body))
    fn[x$body$row == 2 & x$body$col %in% 4:5] <- "pvalString(value)"
    expect_equal(
      sprinkle_fn(x, rows = c(2, 2), cols = 4:5, fn = quote(pvalString(value)),
                  fixed = TRUE)$body$fn,
      fn
    )
  }
)

test_that(
  "Function succeeds when called on a dust_list object",
  {
    expect_silent(
      dplyr::group_by(mtcars, am, vs) %>% 
        dust(ungroup = FALSE) %>% 
        sprinkle_fn(cols = "mpg", fn = quote(median(value)))
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Casts an error if x is not a dust object.",
  {
    expect_error(sprinkle_fn(mtcars))
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Casts an error if bg is not a call object",
  {
    expect_error(sprinkle_fn(x, fn = mtcars))
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Casts an error if part is not one of body, head, foot, or interfoot",
  {
    expect_error(sprinkle_fn(x, fn = NULL, part = "not_a_part"))
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "Casts an error if fixed is not a logical(1)",
  {
    expect_error(sprinkle_fn(x, fn = quote(identity(value)), fixed = "yes"))
  }
)

test_that(
  "Casts an error if fixed is not a logical(1)",
  {
    expect_error(sprinkle_fn(x, fn = quote(identity(value)), 
                             fixed = c(TRUE, FALSE)))
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "Casts an error if recycle is not one of none, rows, or cols",
  {
    expect_error(sprinkle_fn(x, fn = quote(identity(value)), recycle = "not_an_option"))
  }
)
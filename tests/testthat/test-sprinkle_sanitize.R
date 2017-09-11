context("sprinkle_sanitize")

x <- dust(head(mtcars))

# Functional Requirement 1 ------------------------------------------

test_that(
  "Correctly reassigns the appropriate elements of sanitize and
  sanitize_args columns in the table part",
  {
    expect_equal(
      sprinkle_sanitize(x, cols = 2, sanitize = TRUE)[["body"]][["sanitize"]],
      rep(c(FALSE, TRUE, FALSE), times = c(6, 6, 6*9))
    )
  }  
)

test_that(
  "Correctly reassigns the appropriate elements of sanitize and
  sanitize_units columns in the table part",
  {
    expect_equal(
      sprinkle_sanitize(x, cols = 2, sanitize_args = list(greek = TRUE))[["body"]][["sanitize_args"]],
      rep(c("", 
            deparse(list(greek = TRUE)), 
            ""), 
          times = c(6, 6, 6*9))
    )
  }  
)

test_that(
  "Function succeeds when called on a dust_list object",
  {
    expect_silent(
      dplyr::group_by(mtcars, am, vs) %>% 
        dust(ungroup = FALSE) %>% 
        sprinkle_sanitize(sanitize = TRUE)
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Cast an error if x is not a dust object",
  {
    expect_error(sprinkle_sanitize(mtcars))
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Cast an error if sanitize is not a logical(1)",
  {
    expect_error(sprinkle_sanitize(x, sanitize = "TRUE"))
  }
)

test_that(
  "Cast an error if sanitize is not a logical(1)",
  {
    expect_error(sprinkle_sanitize(x, sanitize = c(TRUE, FALSE)))
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Cast an error if sanitize_args is not a list",
  {
    expect_error(sprinkle_sanitize(x, sanitize_args = 20))
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "Casts an error if part is not one of body, head, foot, or interfoot",
  {
    expect_error(
      sprinkle_sanitize(x, sanitize = 20, part = "not a part")
    )
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "Casts an error if fixed is not logical(1)",
  {
    expect_error(sprinkle_sanitize(x, sanitize = 20, fixed = "FALSE"))
  }
)

test_that(
  "Casts an error if fixed is not logical(1)",
  {
    expect_error(sprinkle_sanitize(x, sanitize = 20, fixed = c(TRUE, FALSE)))
  }
)

# Functional Requirement 7 ------------------------------------------

test_that(
  "Casts an error if recycle is not one of none, rows, or cols",
  {
    expect_error(sprinkle_sanitize(x, recycle = "not an option"))
  }
)
context("sprinkle_replace")

x <- dust(head(mtcars))

# Functional Requirement 1 ------------------------------------------

test_that(
  "Correctly reassigns the appropriate elements of the replace column in the table part.",
  {
    replace <- rep(NA, nrow(x$body))
    replace[x$body$row == 1] <- "red"
    expect_equal(
      sprinkle_replace(x, rows = 1, replace = "red")$body$replace,
      replace
    )
  }
)

test_that(
  "Correctly reassigns the appropriate elements bg column in the table part.",
  {
    replace <- rep(NA, nrow(x$body))
    replace[x$body$row == 2 & x$body$col %in% 4:5] <- "blue"
    expect_equal(
      sprinkle_replace(x, rows = 2, cols = 4:5, replace = "blue")$body$replace,
      replace
    )
  }
)

test_that(
  "Correctly reassigns the appropriate elements of replace column in the table part.",
  {
    replace <- rep(NA, nrow(x$body))
    replace[x$body$row == 2 & x$body$col %in% 4:5] <- "transparent"
    expect_equal(
      sprinkle_replace(x, rows = c(2, 2), cols = 4:5, replace = "transparent",
                  fixed = TRUE)$body$replace,
      replace
    )
  }
)

test_that(
  "Function succeeds when called on a dust_list object",
  {
    expect_silent(
      dplyr::group_by(mtcars, am, vs) %>% 
        dust(ungroup = FALSE) %>% 
        sprinkle_replace(rows = 1, cols = 1, replace = "abc")
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Casts an error if x is not a dust object.",
  {
    expect_error(sprinkle_replace(mtcars))
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Casts an error if bg is not an atomic vector",
  {
    expect_error(sprinkle_replace(x, replace = mtcars))
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Casts an error if part is not one of body, head, foot, or interfoot",
  {
    expect_error(sprinkle_replace(x, replace = "red", part = "not_a_part"))
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "Casts an warning if the number of indices to replace is not a 
   multiple of `replace`` ",
  {
    expect_warning(
      sprinkle_replace(x, rows = 1, cols = 1:4,
                       replace = 1:3)
    )
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "Casts an error if `length(replace)` is greater than the 
    number of cells to replace.",
  {
    expect_error(
      sprinkle_replace(x = x,
                       rows = 1,
                       replace = rep("blue", 12))
    )
  }
)
# Functional Requirement 7 ------------------------------------------

test_that(
  "Casts an error if fixed is not a logical(1)",
  {
    expect_error(sprinkle_replace(x, replace = "red", fixed = "yes"))
  }
)

test_that(
  "Casts an error if fixed is not a logical(1)",
  {
    expect_error(sprinkle_replace(x, replace = "red", fixed = c(TRUE, FALSE)))
  }
)

test_that(
  "Casts an error if recycle is not one of none, rows, or cols",
  {
    expect_error(sprinkle_replace(x, replace = "red", recycle = "not_an_option"))
  }
)
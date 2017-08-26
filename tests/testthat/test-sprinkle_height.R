context("sprinkle_height")

x <- dust(head(mtcars))

# Functional Requirement 1 ------------------------------------------

test_that(
  "Correctly reassigns the appropriate elements of height and
   height_units columns in the table part",
  {
    expect_equal(
      sprinkle_height(x, cols = 2, height = 20)[["body"]][["height"]],
      rep(c("", "20", ""), times = c(6, 6, 6*9))
    )
  }  
)

test_that(
  "Correctly reassigns the appropriate elements of height and
   height_units columns in the table part",
  {
    expect_equal(
      sprinkle_height(x, cols = 2, height_units = "%")[["body"]][["height_units"]],
      rep(c("", "%", ""), times = c(6, 6, 6*9))
    )
  }  
)

test_that(
  "Function succeeds when called on a dust_list object",
  {
    expect_silent(
      dplyr::group_by(mtcars, am, vs) %>% 
        dust(ungroup = FALSE) %>% 
        sprinkle_height(height = 10)
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Cast an error if x is not a dust object",
  {
    expect_error(sprinkle_height(mtcars))
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Cast an error if height is not a numeric(1)",
  {
    expect_error(sprinkle_height(x, height = "20"))
  }
)

test_that(
  "Cast an error if height is not a numeric(1)",
  {
    expect_error(sprinkle_height(x, height = c(20, 40)))
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Cast an error if height_units is not a character(1)",
  {
    expect_error(sprinkle_height(x, height_units = 20))
  }
)

test_that(
  "Cast an error if height_units is not a character(1)",
  {
    expect_error(sprinkle_height(x, height = c("px", "pt")))
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "Casts an error if part is not one of body, head, foot, or interfoot",
  {
    expect_error(
      sprinkle_height(x, height = 20, part = "not a part")
    )
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "Casts an error if fixed is not logical(1)",
  {
    expect_error(sprinkle_height(x, height = 20, fixed = "FALSE"))
  }
)

test_that(
  "Casts an error if fixed is not logical(1)",
  {
    expect_error(sprinkle_height(x, height = 20, fixed = c(TRUE, FALSE)))
  }
)

# Functional Requirement 7 ------------------------------------------

test_that(
  "Casts an error if recycle is not one of none, rows, or cols",
  {
    expect_error(sprinkle_height(x, recycle = "not an option"))
  }
)
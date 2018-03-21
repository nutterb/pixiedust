context("sprinkle_width")

x <- dust(head(mtcars))

# Functional Requirement 1 ------------------------------------------

test_that(
  "Correctly reassigns the appropriate elements of width and
   width_units columns in the table part",
  {
    expect_equal(
      sprinkle_width(x, cols = 2, width = 20)[["body"]][["width"]],
      rep(c("", "20", ""), times = c(6, 6, 6*9))
    )
  }  
)

test_that(
  "Correctly reassigns the appropriate elements of width and
   width_units columns in the table part",
  {
    expect_equal(
      sprinkle_width(x, cols = 2, width_units = "%")[["body"]][["width_units"]],
      rep(c("", "%", ""), times = c(6, 6, 6*9))
    )
  }  
)

test_that(
  "Function succeeds when called on a dust_list object",
  {
    expect_silent(
      mtcars %>% 
        dplyr::group_by(am, vs) %>% 
        dust(ungroup = FALSE) %>% 
        sprinkle_width(cols = "mpg", 
                       width = 150,
                       width_units = "pt")
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Cast an error if x is not a dust object",
  {
    expect_error(sprinkle_width(mtcars))
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Cast an error if width is not a numeric(1)",
  {
    expect_error(sprinkle_width(x, width = "20"))
  }
)

test_that(
  "Cast an error if width is not a numeric(1)",
  {
    expect_error(sprinkle_width(x, width = c(20, 40)))
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Cast an error if width_units is not a character(1)",
  {
    expect_error(sprinkle_width(x, width_units = 20))
  }
)

test_that(
  "Cast an error if width_units is not a character(1)",
  {
    expect_error(sprinkle_width(x, width = c("px", "pt")))
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "Casts an error if part is not one of body, head, foot, or interfoot",
  {
    expect_error(
      sprinkle_width(x, width = 20, part = "not a part")
    )
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "Casts an error if fixed is not logical(1)",
  {
    expect_error(sprinkle_width(x, width = 20, fixed = "FALSE"))
  }
)

test_that(
  "Casts an error if fixed is not logical(1)",
  {
    expect_error(sprinkle_width(x, width = 20, fixed = c(TRUE, FALSE)))
  }
)

# Functional Requirement 7 ------------------------------------------

test_that(
  "Casts an error if recycle is not one of none, rows, or cols",
  {
    expect_error(sprinkle_width(x, recycle = "not an option"))
  }
)

# Functional Requirement 8 ------------------------------------------

test_that(
  "Casts an error if recycle = 'none' and width does not have length 1",
  {
    expect_error(sprinkle_width(x, recycle = "none", width = 1:2))
  }
)

# Functional Requirement 9 ------------------------------------------

test_that(
  "Correctly assigns values when recycle is not 'none' and multiple values are given.",
  {
    expect_equal(
      sprinkle_width(x, 
                     cols = 1:2,
                     width = c(1, 3),
                     recycle = "rows")[["body"]][["width"]][1:12],
      rep(c("1", "3"), each = 6)
    )
  }
)

test_that(
  "Correctly assigns values when recycle is not 'none' and multiple values are given.",
  {
    expect_equal(
      sprinkle_width(x, 
                     cols = 1:2,
                     width = c(1, 3),
                     recycle = "cols")[["body"]][["width"]][1:12],
      rep(c("1", "3"), times = 6)
    )
  }
)

# Functional Requirement 10 -----------------------------------------

test_that(
  "Quietly accepts only the first value in width_units when recycle = 'none'.",
  {
    expect_equal(
      sprinkle_width(x,
                     cols = 1:2,
                     width = 2,
                     width_units = c("in", "pt"),
                     recycle = "none")[["body"]][["width_units"]][1:12],
      rep("in", 12)
    )
  }
)
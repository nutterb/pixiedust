context("sprinkle_gradient")

x <- dust(mtcars)

color_range <- 
  scales::gradient_n_pal(c("#132B43", "#56B1F7"))(seq(0, 1, 
                                                      length.out = 10))
color_index <- 
  as.numeric(cut(mtcars$mpg,
                 breaks = quantile(mtcars$mpg, 
                                   probs = seq(0, 1, length.out = 10),
                                   na.rm = TRUE),
                 include.lowest = TRUE))

# Functional Requirement 1 ------------------------------------------

test_that(
  "Correctly reassigns the appropriate elements of the bg column",
  {
    expect_equal(
      sprinkle_gradient(
        x, 
        cols = "mpg", 
        gradient = "bg")[["body"]][["bg"]][1:32],
      
      color_range[color_index]
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Casts an error if x is not a dust object",
  {
    expect_error(
      sprinkle_gradient(mtcars)
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Cast an error if gradient is not a subset of ...",
  {
    expect_error(
      sprinkle_gradient(x, gradient = "not an option"),
      "Must be a subset of"
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Cast an error if gradient_colors is not a character(2)",
  {
    expect_error(
      sprinkle_gradient(x, gradient_colors = 1:2),
      "Must be of type 'character'"
    )
  }
)

test_that(
  "Cast an error if gradient_colors is not a character(2)",
  {
    expect_error(
      sprinkle_gradient(x, gradient_colors = "black"),
      "Must have length 2"
    )
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "Cast an error if gradient_colors is not a recognized color value",
  {
    expect_error(
      sprinkle_gradient(x, gradient_colors = c("rgb(-3,520,17)",
                                               "some other color")),
      "are not valid colors"
    )
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "Cast an error if gradient_cut is not numeric",
  {
    expect_error(
      sprinkle_gradient(x, cols = "mpg", gradient_cut = "10"),
      "Must be of type 'numeric'"
    )
  }
)

# Functional Requirement 7 ------------------------------------------

test_that(
  "Cast an error if gradient_n is not numeric(1)",
  {
    expect_error(
      sprinkle_gradient(x, gradient_n = c(5, 10)),
      "Must have length 1"
    )
  }
)

test_that(
  "Cast an error if gradient_n is not numeric(1)",
  {
    expect_error(
      sprinkle_gradient(x, gradient_n = "10"),
      "Must be of type 'numeric'"
    )
  }
)

# Functional Requirement 8 ------------------------------------------

test_that(
  "Cast an error if gradient_na is not character(1)",
  {
    expect_error(
      sprinkle_gradient(x, gradient_na = 7),
      "Must be of type 'character'"
    )
  }
)

test_that(
  "Cast an error if gradient_na is not character(1)",
  {
    expect_error(
      sprinkle_gradient(x, gradient_na = c("red", "blue")),
      "Must have length 1"
    )
  }
)

# Functional Requirement 9 ------------------------------------------

test_that(
  "Cast an error if gradient_na is not a valid color",
  {
    expect_error(
      sprinkle_gradient(x, gradient_na = "some other color")
    )
  }
)

# Functional Requirement 10 -----------------------------------------

test_that(
  "Cast an error if part is not one of body, head, foot, interfoot",
  {
    expect_error(
      sprinkle_gradient(x = x,
                        cols = "gear",
                        part = "not a part")
    )
  }
)

# Functional Requirement 11 ------------------------------------------

test_that(
  "Cast an error if fixed is not logical(1)",
  expect_error(
    sprinkle_gradient(x = x,
                      cols = "gear",
                      fixed = "FALSE")
  )
)

test_that(
  "Cast an error if fixed is not logical(1)",
  expect_error(
    sprinkle_gradient(x = x,
                      cols = "gear",
                      fixed = c(TRUE, FALSE))
  )
)

# Functional Requirement 12 ------------------------------------------

test_that(
  "Cast an error if recycle is not one of none, rows, cols",
  {
    expect_error(
      sprinkle_gradient(x = x,
                        cols = "gear",
                        recycle = "not a value")
    )
  }
)

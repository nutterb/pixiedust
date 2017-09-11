context("sprinkle_discrete")

x <- dust(mtcars)

# Functional Requirement 1 ------------------------------------------

test_that(
  "Correctly reassigns `bg`",
  {
    expect_equal(
      sprinkle_discrete(
        x, 
        cols = "gear", 
        discrete = "bg", 
        discrete_color = c("red", "blue", "green"))[["body"]][["bg"]][289:320],
      c("red", "blue", "green")[match(mtcars$gear, c(3, 4, 5))]
    )
  }
)

test_that(
  "Correctly reassigns `font_color`",
  {
    expect_equal(
      sprinkle_discrete(
        x, 
        cols = "gear", 
        discrete = "font", 
        discrete_color = c("red", "blue", "green"))[["body"]][["font_color"]][289:320],
      c("red", "blue", "green")[match(mtcars$gear, c(3, 4, 5))]
    )
  }
)

test_that(
  "Correctly reassigns `font_color`",
  {
    expect_equal(
      sprinkle_discrete(
        x, 
        cols = "gear", 
        discrete = "font_color", 
        discrete_color = c("red", "blue", "green"))[["body"]][["font_color"]][289:320],
      c("red", "blue", "green")[match(mtcars$gear, c(3, 4, 5))]
    )
  }
)

test_that(
  "Selects default colors when discrete_color is NULL",
  {
    expect_silent(
      sprinkle_discrete(x, cols = "gear", discrete = "bg")
    )
  }
)

test_that(
  "Correctly reassigns `border`",
  {
    expect_equal(
      sprinkle_discrete(
        x, 
        cols = "gear", 
        discrete = "border", 
        discrete_color = c("red", "blue", "green"))[["body"]][["left_border"]][289:320],
      sprintf("1px solid %s", c("red", "blue", "green")[match(mtcars$gear, c(3, 4, 5))])
    )
  }
)

test_that(
  "Correctly reassigns `border`",
  {
    expect_equal(
      sprinkle_discrete(
        x, 
        cols = "gear", 
        discrete = "right_border", 
        discrete_color = c("red", "blue", "green"))[["body"]][["right_border"]][289:320],
      sprintf("1px solid %s", c("red", "blue", "green")[match(mtcars$gear, c(3, 4, 5))])
    )
  }
)

test_that(
  "Function succeeds when called on a dust_list object",
  {
    expect_silent(
      dplyr::group_by(mtcars, am, vs) %>% 
        dust(ungroup = FALSE) %>% 
        sprinkle_discrete(cols = "gear",
                          discrete = "bg",
                          discrete_color = c("red", "blue", "green"))
    )
  }
)


# Functional Requirement 2 ------------------------------------------

test_that(
  "Cast an error if x is not a dust object",
  {
    expect_error(sprinkle_discrete(x = mtcars,
                                   cols = "gear",
                                   discrete = "bg"))
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Cast an error if discrete is not a subset of bg, border, font, ...",
  {
    expect_error(sprinkle_discrete(x = x,
                                   cols = "gear",
                                   discrete = "font_height"))
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Cast an error if discrete_color is not character",
  {
    expect_error(sprinkle_discrete(x = x,
                                   cols = "gear",
                                   discrete_colors = 1:3))
  }
)

# Functional Requirement 5 ------------------------------------------
test_that(
  "Cast an error if discrete_color is not a recognized color",
  {
    expect_error(sprinkle_discrete(x = x,
                                   cols = "gear",
                                   discrete_colors = c("my own red",
                                                       "my own blue",
                                                       "my own green")))
  }
)

test_that(
  "Cast an error if discrete_color has too few values",
  {
    expect_error(sprinkle_discrete(x = x,
                                   cols = "gear",
                                   discrete_colors = c('red', 'blue')))
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "Cast an error if part is not one of body, head, foot, interfoot",
  {
    expect_error(
      sprinkle_discrete(x = x,
                        cols = "gear",
                        part = "not a part")
    )
  }
)

# Functional Requirement 7 ------------------------------------------

test_that(
  "Cast an error if fixed is not logical(1)",
  expect_error(
    sprinkle_discrete(x = x,
                      cols = "gear",
                      fixed = "FALSE")
  )
)

test_that(
  "Cast an error if fixed is not logical(1)",
  expect_error(
    sprinkle_discrete(x = x,
                      cols = "gear",
                      fixed = c(TRUE, FALSE))
  )
)

# Functional Requirement 8 ------------------------------------------

test_that(
  "Cast an error if recycle is not one of none, rows, cols",
  {
    expect_error(
      sprinkle_discrete(x = x,
                        cols = "gear",
                        recycle = "not a value")
    )
  }
)

context("sprinkle_border_collapse")

x <- dust(mtcars)

# Functional Requirement 1 ------------------------------------------

test_that(
  "Change the border_collapse attribute of the dust object",
  {
    expect_equal(
      sprinkle_border_collapse(x, "separate")[["border_collapse"]],
      "separate"
    )
  }
)

test_that(
  "Function succeeds when called on a dust_list object",
  {
    expect_silent(
      dplyr::group_by(mtcars, am, vs) %>% 
        dust(ungroup = FALSE) %>% 
        sprinkle_border_collapse("separate")
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Cast an error if x is not a dust object",
  {
    expect_error(
      sprinkle_border_collapse(mtcars)
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Cast an error if x is not one of the acceptable values",
  {
    expect_error(
      sprinkle_border_collapse(x, border_collapse = "some value")
    )
  }
)

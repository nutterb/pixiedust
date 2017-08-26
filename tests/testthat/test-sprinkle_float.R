context("sprinkle_float")

x <- dust(mtcars)

# Functional Requirement 1 ------------------------------------------

test_that(
  "Change the float attribute of the dust object",
  {
    expect_equal(
      sprinkle_float(x, TRUE)[["float"]],
      TRUE
    )
  }
)

test_that(
  "Function succeeds when called on a dust_list object",
  {
    expect_silent(
      dplyr::group_by(mtcars, am, vs) %>% 
        dust(ungroup = FALSE) %>% 
        sprinkle_float(float = TRUE)
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Cast an error if x is not a dust object",
  {
    expect_error(
      sprinkle_float(mtcars)
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Cast an error if x is a logical object with length not equal to 1",
  {
    expect_error(
      sprinkle_float(x, c(FALSE, TRUE))
    )
  }
)

test_that(
  "Cast an error if x is not a logical object",
  {
    expect_error(
      sprinkle_float(x, "TRUE")
    )
  }
)

context("sprinkle_hhline")

x <- dust(mtcars)

# Functional Requirement 1 ------------------------------------------

test_that(
  "Change the hhline attribute of the dust object",
  {
    expect_equal(
      sprinkle_hhline(x, TRUE)[["hhline"]],
      TRUE
    )
  }
)

test_that(
  "Succeeds when called on a dust_list object",
  {
    expect_silent(
      dplyr::group_by(mtcars, am, vs) %>% 
        dust(ungroup = FALSE) %>% 
        sprinkle_hhline(hhline = FALSE)
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Cast an error if x is not a dust object",
  {
    expect_error(
      sprinkle_hhline(mtcars)
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Cast an error if x is a logical object with length not equal to 1",
  {
    expect_error(
      sprinkle_hhline(x, c(TRUE, FALSE))
    )
  }
)

test_that(
  "Cast an error if x is not a logical object",
  {
    expect_error(
      sprinkle_hhline(x, "TRUE")
    )
  }
)

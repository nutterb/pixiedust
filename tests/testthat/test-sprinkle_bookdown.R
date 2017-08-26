context("sprinkle_bookdown")

x <- dust(mtcars)

# Functional Requirement 1 ------------------------------------------

test_that(
  "Change the bookdown attribute of the dust object",
  {
    expect_equal(
      sprinkle_bookdown(x, TRUE)[["bookdown"]],
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
        sprinkle_bookdown(bookdown = TRUE)
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Cast an error if x is not a dust object",
  {
    expect_error(
      sprinkle_bookdown(mtcars)
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Cast an error if x is not a logical object",
  {
    expect_error(
      sprinkle_bookdown(x, "FALSE")
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Cast an error if x has length greater than 1",
  {
    expect_error(
      sprinkle_bookdown(x, c(TRUE, FALSE))
    )
  }
)
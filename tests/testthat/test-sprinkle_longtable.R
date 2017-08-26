context("sprinkle_longtable")

x <- dust(mtcars)

# Functional Requirement 1 ------------------------------------------

test_that(
  "Change the longtable attribute of the dust object",
  {
    expect_equal(
      sprinkle_longtable(x, TRUE)[["longtable"]],
      TRUE
    )
  }
)


test_that(
  "Change the longtable attribute of the dust object",
  {
    expect_equal(
      sprinkle_longtable(x, 15)[["longtable"]],
      15
    )
  }
)

test_that(
  "Function succeeds when called on a dust_list object",
  {
    expect_silent(
      dplyr::group_by(mtcars, am, vs) %>% 
        dust(ungroup = FALSE) %>% 
        sprinkle_longtable(longtable = TRUE)
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Cast an error if x is not a dust object",
  {
    expect_error(
      sprinkle_longtable(mtcars)
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Cast an error if x is a logical object with length not equal to 1",
  {
    expect_error(
      sprinkle_longtable(x, c(FALSE, TRUE))
    )
  }
)

test_that(
  "Cast an error if x is a logical object with length not equal to 1",
  {
    expect_error(
      sprinkle_longtable(x, logical(0))
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Cast an error if x is not integerish or has length not equal to 1",
  {
    expect_error(
      sprinkle_longtable(x, c(10, 15))
    )
  }
)

test_that(
  "Cast an error if x is not integerish or has length not equal to 1",
  {
    expect_error(
      sprinkle_longtable(x, numeric(0))
    )
  }
)
context("sprinkle_tabcolsep")

x <- dust(mtcars)

# Functional Requirement 1 ------------------------------------------

test_that(
  "Change the tabcolsep attribute of the dust object",
  {
    expect_equal(
      sprinkle_tabcolsep(x, 4)[["tabcolsep"]],
      4
    )
  }
)

test_that(
  "Function succeeds when called on a dust_list object",
  {
    expect_silent(
      dplyr::group_by(mtcars, am, vs) %>% 
        dust(ungroup = FALSE) %>% 
        sprinkle_tabcolsep(tabcolsep = 3)
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Cast an error if x is not a dust object",
  {
    expect_error(
      sprinkle_tabcolsep(mtcars)
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Cast an error if x is a logical object with length not equal to 1",
  {
    expect_error(
      sprinkle_tabcolsep(x, 1:2)
    )
  }
)

test_that(
  "Cast an error if x is not a logical object",
  {
    expect_error(
      sprinkle_tabcolsep(x, "TRUE")
    )
  }
)

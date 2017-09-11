context("sprinkle_justify")

x <- dust(mtcars)

# Functional Requirement 1 ------------------------------------------

test_that(
  "Change the justify attribute of the dust object: left",
  {
    expect_equal(
      sprinkle_justify(x, "left")[["justify"]],
      "left"
    )
  }
)

test_that(
  "Change the justify attribute of the dust object: center",
  {
    expect_equal(
      sprinkle_justify(x, "center")[["justify"]],
      "center"
    )
  }
)

test_that(
  "Change the justify attribute of the dust object: right",
  {
    expect_equal(
      sprinkle_justify(x, "right")[["justify"]],
      "right"
    )
  }
)

test_that(
  "Function succeeds when called on a dust_list object",
  {
    expect_silent(
      dplyr::group_by(mtcars, am, vs) %>% 
        dust(ungroup = FALSE) %>% 
        sprinkle_justify(justify = "left")
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Cast an error if x is not a dust object",
  {
    expect_error(
      sprinkle_justify(mtcars)
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Cast an error if justify is not one of center, left, or right",
  {
    expect_error(
      sprinkle_justify(x, "lft misspelled")
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Ignore capitalization of the justify argument",
  {
    expect_silent(
      sprinkle_justify(x, "Left")
    )
  }
)

test_that(
  "Ignore capitalization of the justify argument",
  {
    expect_silent(
      sprinkle_justify(x, "LeFt")
    )
  }
)

test_that(
  "Ignore capitalization of the justify argument",
  {
    expect_silent(
      sprinkle_justify(x, "R")
    )
  }
)

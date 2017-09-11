context("sprinkle_label")

x <- dust(mtcars)

# Functional Requirement 1 ------------------------------------------

test_that(
  "Change the label attribute of the dust object",
  {
    expect_equal(
      sprinkle_label(x, "separate")[["label"]],
      "separate"
    )
  }
)

test_that(
  "Function succeeds when called on a dust_list object",
  {
    expect_warning(
      dplyr::group_by(mtcars, am, vs) %>% 
        dust(ungroup = FALSE) %>% 
        sprinkle_label(label = "label")
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Cast an error if x is not a dust object",
  {
    expect_error(
      sprinkle_label(mtcars)
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Cast an error if x is not a character(1)",
  {
    expect_error(
      sprinkle_label(x, label = character(0))
    )
  }
)

test_that(
  "Cast an error if x is not a character(1)",
  {
    expect_error(
      sprinkle_label(x, label = letters[1:2])
    )
  }
)

test_that(
  "Cast an error if x is not a character(1)",
  {
    expect_error(
      sprinkle_label(x, label = 1)
    )
  }
)
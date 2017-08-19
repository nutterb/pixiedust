context("sprinkle_caption")

x <- dust(mtcars)

# Functional Requirement 1 ------------------------------------------

test_that(
  "Change the caption attribute of the dust object",
  {
    expect_equal(
      sprinkle_caption(x, "new caption")[["caption"]],
      "new caption"
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Cast an error if x is not a dust object",
  {
    expect_error(
      sprinkle_caption(mtcars)
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Cast an error if x is not a character object",
  {
    expect_error(
      sprinkle_caption(x, 1234)
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Cast an error if x has length greater than 1",
  {
    expect_error(
      sprinkle_caption(x, letters[1:2])
    )
  }
)
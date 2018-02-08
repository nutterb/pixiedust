context("sprinkle_caption_number")

x <- dust(mtcars)

# Functional Requirement 1 ------------------------------------------

test_that(
  "Change the caption_number attribute of the dust object",
  {
    expect_equal(
      sprinkle_caption_number(x, FALSE)[["caption_number"]],
      FALSE
    )
  }
)

test_that(
  "Function succeeds when called on a dust_list object",
  {
    expect_silent(
      dplyr::group_by(mtcars, am, vs) %>% 
        dust(ungroup = FALSE) %>% 
        sprinkle_caption_number(FALSE)
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Cast an error if x is not a dust object",
  {
    expect_error(
      sprinkle_caption_number(mtcars)
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Cast an error if caption_number is not a logical object",
  {
    expect_error(
      sprinkle_caption_number(x, 1234)
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Cast an error if caption_number has length greater than 1",
  {
    expect_error(
      sprinkle_caption_number(x, c(TRUE, FALSE))
    )
  }
)
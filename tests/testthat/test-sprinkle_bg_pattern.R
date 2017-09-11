context("sprinkle_bg_pattern")

x <- dust(head(mtcars))

# Functional Requirement 1 ------------------------------------------

test_that(
  "Correctly reassigns the appropriate elements of the `bg` columns 
   in the table part",
  {
    expect_equal(
      sprinkle_bg_pattern(x = x,
                          bg_pattern = c('white', 'black'))[["body"]][["bg"]],
      rep(c("white", "black"), length.out = 66)
    )
  }
)

test_that(
  "Correctly reassigns the appropriate elements of the `bg` columns 
   in the table part",
  {
    expect_equal(
      sprinkle_bg_pattern(x = x,
                          rows = 1:2,
                          bg_pattern = c('white', 'black'))[["body"]][["bg"]],
      rep(c("white", "black", rep("", 4)), length.out = 66)
    )
  }
)

test_that(
  "Correctly reassigns the appropriate elements of the `bg` columns 
  in the table part",
  {
    expect_equal(
      sprinkle_bg_pattern(x = x,
                          bg_pattern = c('white', 'black'),
                          bg_pattern_by = "cols")[["body"]][["bg"]],
      rep(rep(c("white", "black"), length.out = 11),
          rep(6, 11))
    )
  }
)

test_that(
  "Function succeeds when called on a dust_list object",
  {
    expect_silent(
      dplyr::group_by(mtcars, am, vs) %>% 
        dust(ungroup = FALSE) %>% 
        sprinkle_bg_pattern(bg_pattern = c("white", "black"))
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Casts an error if x is not a dust object",
  {
    expect_error(
      sprinkle_bg_pattern(mtcars)
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Casts an error if `bg_pattern` is not a character vector",
  {
    expect_error(
      sprinkle_bg_pattern(x = x,
                          bg_pattern = c(TRUE, FALSE))
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Casts an error if any element of `bg_pattern` is not a valid color",
  {
    expect_error(
      sprinkle_bg_pattern(x = x,
                          bg_pattern = c("white", "blue", "not actually a color"))
    )
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "Casts an error if `bg_pattern_by` is not a subset of [rows, columns]",
  {
    expect_error(
      sprinkle_bg_pattern(x = x,
                          bg_pattern_by = "Column")
    )
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "Casts an error if `part` is not one of [body, head, foot, interfoot]",
  {
    expect_error(
      sprinkle_bg_pattern(x = x,
                          part = "not a part")
    )
  }
)

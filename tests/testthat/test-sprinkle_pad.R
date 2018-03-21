context("sprinkle_pad")

x <- dust(head(mtcars))

# Functional Requirement 1 ------------------------------------------

test_that(
  "Correctly reassigns the appropriate elements pad column in the table part.",
  {
    pad <- rep("", nrow(x$body))
    pad[x$body$row == 1] <- "3"
    expect_equal(
      sprinkle_pad(x, rows = 1, pad = 3)$body$pad,
      pad
    )
  }
)

test_that(
  "Correctly reassigns the appropriate elements pad column in the table part.",
  {
    pad <- rep("", nrow(x$body))
    pad[x$body$row == 2 & x$body$col %in% 4:5] <- 2
    expect_equal(
      sprinkle_pad(x, rows = 2, cols = 4:5, pad = 2)$body$pad,
      pad
    )
  }
)

test_that(
  "Correctly reassigns the appropriate elements pad column in the table part.",
  {
    pad <- rep("", nrow(x$body))
    pad[x$body$row == 2 & x$body$col %in% 4:5] <- 10
    expect_equal(
      sprinkle_pad(x, rows = c(2, 2), cols = 4:5, pad = 10,
                  fixed = TRUE)$body$pad,
      pad
    )
  }
)

test_that(
  "Function succeeds when called on a dust_list object",
  {
    expect_silent(
      dplyr::group_by(mtcars, am, vs) %>% 
        dust(ungroup = FALSE) %>% 
        sprinkle_pad(pad = 5)
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Casts an error if x is not a dust object.",
  {
    expect_error(sprinkle_pad(mtcars, pad = 3))
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Casts an error if pad is not a numeric(1)",
  {
    expect_error(sprinkle_pad(x, pad = 1:2))
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Casts an error if part is not one of body, head, foot, or interfoot",
  {
    expect_error(sprinkle_pad(x, pad = 2, part = "not_a_part"))
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "Casts an error if fixed is not a logical(1)",
  {
    expect_error(sprinkle_pad(x, pad = 3, fixed = "yes"))
  }
)

test_that(
  "Casts an error if fixed is not a logical(1)",
  {
    expect_error(sprinkle_pad(x, pad = 3, fixed = c(TRUE, FALSE)))
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "Casts an error if recycle is not one of none, rows, or cols",
  {
    expect_error(sprinkle_pad(x, pad = 2, recycle = "not_an_option"))
  }
)

# Functional Requirement 7 -----------------------------------------

test_that(
  "Casts an error if recycle = 'none' and pad does not have length 1.",
  {
    expect_error(sprinkle_pad(x, pad = 1:3, recycle = "none"))
  }
)

test_that(
  "Passes when recycle != 'none' and pad does has length > 1.",
  {
    expect_silent(sprinkle_pad(x, pad = 1:3, recycle = "rows"))
  }
)
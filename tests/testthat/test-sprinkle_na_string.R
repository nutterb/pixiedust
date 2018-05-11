context("sprinkle_na_string")

x <- dust(head(mtcars))

# Functional Requirement 1 ------------------------------------------

test_that(
  "Correctly reassigns the appropriate elements of the na_string column in the table part.",
  {
    na_string <- rep(NA, nrow(x$body))
    na_string[x$body$row == 1] <- "red"
    expect_equal(
      sprinkle_na_string(x, rows = 1, na_string = "red")$body$na_string,
      na_string
    )
  }
)

test_that(
  "Correctly reassigns the appropriate elements bg column in the table part.",
  {
    na_string <- rep(NA, nrow(x$body))
    na_string[x$body$row == 2 & x$body$col %in% 4:5] <- "blue"
    expect_equal(
      sprinkle_na_string(x, rows = 2, cols = 4:5, na_string = "blue")$body$na_string,
      na_string
    )
  }
)

test_that(
  "Correctly reassigns the appropriate elements bg column in the table part.",
  {
    na_string <- rep(NA, nrow(x$body))
    na_string[x$body$row == 2 & x$body$col %in% 4:5] <- "transparent"
    expect_equal(
      sprinkle_na_string(x, rows = c(2, 2), cols = 4:5, na_string = "transparent",
                  fixed = TRUE)$body$na_string,
      na_string
    )
  }
)

test_that(
  "Succeeds when called on a dust_list object",
  {
    expect_silent(
      dplyr::group_by(mtcars, am, vs) %>% 
        dust(ungroup = FALSE) %>% 
        sprinkle_na_string(na_string = "transparent")
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Casts an error if x is not a dust object.",
  {
    expect_error(sprinkle_na_string(mtcars))
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Casts an error if bg is not a character(1)",
  {
    expect_error(sprinkle_na_string(x, na_string = c("red", "blue")))
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Casts an error if part is not one of body, head, foot, or interfoot",
  {
    expect_error(sprinkle_na_string(x, na_string = "red", part = "not_a_part"))
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "Casts an error if fixed is not a logical(1)",
  {
    expect_error(sprinkle_na_string(x, na_string = "red", fixed = "yes"))
  }
)

test_that(
  "Casts an error if fixed is not a logical(1)",
  {
    expect_error(sprinkle_na_string(x, na_string = "red", fixed = c(TRUE, FALSE)))
  }
)

test_that(
  "Casts an error if recycle is not one of none, rows, or cols",
  {
    expect_error(sprinkle_na_string(x, na_string = "red", recycle = "not_an_option"))
  }
)

# Functional Requirement 6 -----------------------------------------

test_that(
  "Casts an error if recycle = 'none' and na_string does not have length 1.",
  {
    expect_error(sprinkle_na_string(x, na_string = c("Roman", "Arial"), recycle = "none"))
  }
)

test_that(
  "Passes when recycle != 'none' and na_string does has length > 1.",
  {
    expect_silent(sprinkle_na_string(x, na_string = c("Roman", "Arial"), recycle = "rows"))
  }
)
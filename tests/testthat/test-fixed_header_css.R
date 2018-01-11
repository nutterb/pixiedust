context("fixed_header_css.R")

# Functional Requirement 1 ------------------------------------------

test_that(
  "If pretty = TRUE, print results to the console",
  {
    expect_output(fixed_header_css())
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "If pretty = FALSE, Return a character string of length 1",
  {
    checkmate::expect_character(fixed_header_css(pretty = FALSE),
                                len = 1)
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Cast an error if scroll_body_height is not integerish(1)",
  {
    expect_error(fixed_header_css(scroll_body_height = c(100, 200)),
                 "Must have length 1")
  }
)

test_that(
  "Cast an error if scroll_body_height is not integerish(1)",
  {
    expect_error(fixed_header_css(scroll_body_height = "100"),
                 "Must be of type 'integerish'")
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Cast an error if scroll_body_height_units is not character(1)",
  {
    expect_error(fixed_header_css(scroll_body_height_units = 100),
                 "'character'")
  }
)

test_that(
  "Cast an error if scroll_body_height is not character(1)",
  {
    expect_error(fixed_header_css(scroll_body_height_units = c("pt", "px")),
                 "Must have length 1")
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "Cast an error if scroll_body_background_color is not character(1)",
  {
    expect_error(fixed_header_css(scroll_body_background_color = 100),
                 "Must be of type 'character'")
  }
)

test_that(
  "Cast an error if scroll_body_background_color is not character(1)",
  {
    expect_error(fixed_header_css(scroll_body_background_color = c("orchid", "purple")),
                 "Must have length 1")
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "Cast an error if scroll_body_background_color is not a valid color",
  {
    expect_error(fixed_header_css(scroll_body_background_color = c("not a color")),
                 "is not a valid color")
  }
)

# Functional Requirement 7 ------------------------------------------

test_that(
  "Cast an error if fixed_header_height is not integerish(1)",
  {
    expect_error(fixed_header_css(fixed_header_height = c(100, 200)),
                 "Must have length 1")
  }
)

test_that(
  "Cast an error if fixed_header_height is not integerish(1)",
  {
    expect_error(fixed_header_css(fixed_header_height = "100"))
  }
)

# Functional Requirement 8 -----------------------------------------

test_that(
  "Cast an error if fixed_header_height_units is not character(1)",
  {
    expect_error(fixed_header_css(fixed_header_height_units = 100),
                 "'character'")
  }
)

test_that(
  "Cast an error if fixed_header_height is not character(1)",
  {
    expect_error(fixed_header_css(fixed_header_height_units = c("pt", "px")),
                 "Must have length 1")
  }
)

# Functional Requirement 9 -----------------------------------------

test_that(
  "Cast an error if fixed_header_text_height is not integerish(1)",
  {
    expect_error(fixed_header_css(fixed_header_text_height = c(100, 200)),
                 "Must have length 1")
  }
)

test_that(
  "Cast an error if fixed_header_text_height is not integerish(1)",
  {
    expect_error(fixed_header_css(fixed_header_text_height = "100"))
  }
)

# Functional Requirement 10 -----------------------------------------

test_that(
  "Cast an error if fixed_header_text_height_units is not character(1)",
  {
    expect_error(fixed_header_css(fixed_header_text_height_units = 100),
                 "Must be of type")
  }
)

test_that(
  "Cast an error if fixed_header_text_height is not character(1)",
  {
    expect_error(fixed_header_css(fixed_header_text_height_units = c("pt", "px")),
                 "Must have length 1")
  }
)

# Functional Requirement 11 -----------------------------------------

test_that(
  "Cast an error if fixed_header_background_color is not character(1)",
  {
    expect_error(fixed_header_css(fixed_header_background_color = 100),
                 "Must be of type 'character'")
  }
)

test_that(
  "Cast an error if fixed_header_background_color is not character(1)",
  {
    expect_error(fixed_header_css(fixed_header_background_color = c("orchid", "purple")),
                 "Must have length 1")
  }
)

# Functional Requirement 12 -----------------------------------------

test_that(
  "Cast an error if fixed_header_background_color is not a valid color",
  {
    expect_error(fixed_header_css(fixed_header_background_color = c("not a color")),
                 "is not a valid color")
  }
)

# Functional Requirement 13 -----------------------------------------

test_that(
  "Cast an error if pretty is not logical(1)",
  {
    expect_error(fixed_header_css(pretty = c(TRUE, FALSE)),
                 "Must have length 1")
  }
)

test_that(
  "Cast an error if pretty is not logical(1)",
  {
    expect_error(fixed_header_css(pretty = "TRUE"),
                 "Must be of type 'logical'")
  }
)


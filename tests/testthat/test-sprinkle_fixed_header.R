context("sprinkle_fixed_header.R")

x <- dust(head(mtcars))

# Functional Requirement 1 ------------------------------------------

test_that(
  "Set the fixed_header element of the dust object correctly",
  expect_equal(
    sprinkle_fixed_header(x, fixed_header = FALSE)$fixed_header,
    FALSE
  )
)

test_that(
  "Set the fixed_header element of the dust object correctly",
  expect_equal(
    sprinkle_fixed_header(x, fixed_header = TRUE)$fixed_header,
    TRUE
  )
)

test_that(
  "Works with dust_list object",
  {
    test_df <- rbind(mtcars, mtcars, mtcars)
    test_df <- dplyr::group_by(test_df, am)
    expect_silent(
      dust(test_df, ungroup = FALSE) %>% 
        sprinkle_fixed_header(scroll_body_height = 100) %>% 
        sprinkle_print_method("html")
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Set the include_fixed_header_css element of the dust object correctly",
  expect_equal(
    sprinkle_fixed_header(x, include_fixed_header_css = FALSE)$include_fixed_header_css,
    FALSE
  )
)

test_that(
  "Set the include_fixed_header_css element of the dust object correctly",
  expect_equal(
    sprinkle_fixed_header(x, include_fixed_header_css = TRUE)$include_fixed_header_css,
    TRUE
  )
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Set the fixed_header_param element of the dust object correctly",
  expect_equal(
    sprinkle_fixed_header(x, 
                          fixed_header_class_name = "new-pixie",
                          scroll_body_height = 400,
                          scroll_body_height_units = "pt",
                          scroll_body_background_color = "orchid",
                          fixed_header_height = 30,
                          fixed_header_height_units = "pt",
                          fixed_header_text_height = 30,
                          fixed_header_text_height_units = "pt",
                          fixed_header_background_color = "pink")$fixed_header_param,
    list(fixed_header_class_name = "new-pixie",
         scroll_body_height = 400,
         scroll_body_height_units = "pt",
         scroll_body_background_color = "orchid",
         fixed_header_height = 30,
         fixed_header_height_units = "pt",
         fixed_header_text_height = 30,
         fixed_header_text_height_units = "pt",
         fixed_header_background_color = "pink")
  )
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Cast an error if x does not inherit class dust",
  {
    expect_error(sprinkle_fixed_header(mtcars),
                 "class 'dust'")
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "Cast an error if scroll_body_height is not integerish(1)",
  {
    expect_error(sprinkle_fixed_header(x,
                                       scroll_body_height = c(100, 200)),
                 "Must have length 1")
  }
)

test_that(
  "Cast an error if scroll_body_height is not integerish(1)",
  {
    expect_error(sprinkle_fixed_header(x,
                                       scroll_body_height = "100"),
                 "'integerish'")
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "Cast an error if scroll_body_height_units is not character(1)",
  {
    expect_error(sprinkle_fixed_header(x,
                                       scroll_body_height_units = 100),
                 "'character'")
  }
)

test_that(
  "Cast an error if scroll_body_height is not character(1)",
  {
    expect_error(sprinkle_fixed_header(x,
                                       scroll_body_height_units = c("pt", "px")),
                 "Must have length 1")
  }
)

# Functional Requirement 7 ------------------------------------------

test_that(
  "Cast an error if scroll_body_background_color is not character(1)",
  {
    expect_error(sprinkle_fixed_header(x,
                                       scroll_body_background_color = 100),
                 "'character'")
  }
)

test_that(
  "Cast an error if scroll_body_background_color is not character(1)",
  {
    expect_error(sprinkle_fixed_header(x,
                                       scroll_body_background_color = c("orchid", "purple")),
                 "Must have length 1")
  }
)

# Functional Requirement 8 ------------------------------------------

test_that(
  "Cast an error if scroll_body_background_color is not a valid color",
  {
    expect_error(sprinkle_fixed_header(x,
                                       scroll_body_background_color = c("not a color")),
                 "is not a valid color")
  }
)

# Functional Requirement 9 ------------------------------------------

test_that(
  "Cast an error if fixed_header_height is not integerish(1)",
  {
    expect_error(sprinkle_fixed_header(x,
                                       fixed_header_height = c(100, 200)),
                 "Must have length 1")
  }
)

test_that(
  "Cast an error if fixed_header_height is not integerish(1)",
  {
    expect_error(sprinkle_fixed_header(x,
                                       fixed_header_height = "100"))
  }
)

# Functional Requirement 10 -----------------------------------------

test_that(
  "Cast an error if fixed_header_height_units is not character(1)",
  {
    expect_error(sprinkle_fixed_header(x,
                                       fixed_header_height_units = 100),
                 "'character'")
  }
)

test_that(
  "Cast an error if fixed_header_height is not character(1)",
  {
    expect_error(sprinkle_fixed_header(x,
                                       fixed_header_height_units = c("pt", "px")),
                 "Must have length 1")
  }
)

# Functional Requirement 11 -----------------------------------------

test_that(
  "Cast an error if fixed_header_text_height is not integerish(1)",
  {
    expect_error(sprinkle_fixed_header(x,
                                       fixed_header_text_height = c(100, 200)),
                 "Must have length 1")
  }
)

test_that(
  "Cast an error if fixed_header_text_height is not integerish(1)",
  {
    expect_error(sprinkle_fixed_header(x,
                                       fixed_header_text_height = "100"))
  }
)

# Functional Requirement 12 -----------------------------------------

test_that(
  "Cast an error if fixed_header_text_height_units is not character(1)",
  {
    expect_error(sprinkle_fixed_header(x,
                                       fixed_header_text_height_units = 100),
                 "Must be of type")
  }
)

test_that(
  "Cast an error if fixed_header_text_height is not character(1)",
  {
    expect_error(sprinkle_fixed_header(x,
                                       fixed_header_text_height_units = c("pt", "px")),
                 "Must have length 1")
  }
)

# Functional Requirement 13 -----------------------------------------

test_that(
  "Cast an error if fixed_header_background_color is not character(1)",
  {
    expect_error(sprinkle_fixed_header(x,
                                       fixed_header_background_color = 100),
                 "'character'")
  }
)

test_that(
  "Cast an error if fixed_header_background_color is not character(1)",
  {
    expect_error(sprinkle_fixed_header(x,
                                       fixed_header_background_color = c("orchid", "purple")),
                 "Must have length 1")
  }
)

# Functional Requirement 14 -----------------------------------------

test_that(
  "Cast an error if fixed_header_background_color is not a valid color",
  {
    expect_error(sprinkle_fixed_header(x,
                                       fixed_header_background_color = c("not a color")),
                 "is not a valid color")
  }
)

# Functional Requirement 15 -----------------------------------------

test_that(
  "Cast an error if include_fixed_header_css is not logical(1)",
  {
    expect_error(sprinkle_fixed_header(x,
                                       include_fixed_header_css = c(TRUE, FALSE)),
                 "Must have length 1")
  }
)

test_that(
  "Cast an error if include_fixed_header_css is not logical(1)",
  {
    expect_error(sprinkle_fixed_header(x,
                                       include_fixed_header_css = "TRUE"),
                 "'logical'")
  }
)

# Functional Requirement 16 -----------------------------------------

test_that(
  "Cast an error if fixed_header_class_name is not character(1)",
  {
    expect_error(sprinkle_fixed_header(x,
                                       fixed_header_class_name = 100),
                 "'character'")
  }
)

test_that(
  "Cast an error if fixed_header_class_name is not character(1)",
  {
    expect_error(sprinkle_fixed_header(x,
                                       fixed_header_class_name = c("orchid", "purple")),
                 "Must have length 1")
  }
)

# Functional Requirement 16 -----------------------------------------

test_that(
  "Cast an error if fixed_header is not logical(1)",
  {
    expect_error(sprinkle_fixed_header(x,
                                       fixed_header = c(TRUE, FALSE)),
                 "Must have length 1")
  }
)

test_that(
  "Cast an error if fixed_header is not logical(1)",
  {
    expect_error(sprinkle_fixed_header(x,
                                       fixed_header = "TRUE"),
                 "'logical'")
  }
)

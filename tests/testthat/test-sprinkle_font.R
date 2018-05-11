context("sprinkle_font")

x <- dust(head(mtcars))

# Functional Requirement 1 ------------------------------------------

test_that(
  "Correctly change the bold column of the table part for the selected cells",
  {
    expect_equal(
      sprinkle_font(x = x,
                    bold = TRUE)[["body"]][["bold"]],
      rep(TRUE, 66)
    )
  }
)

test_that(
  "Succeeds when called on a dust_list object",
  {
    expect_silent(
      mtcars %>% 
        dplyr::group_by(vs, am) %>% 
        dust(ungroup = FALSE) %>% 
        sprinkle_font(cols = "mpg", 
                      bold = TRUE)
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Correctly change the italic column of the table part for the selected cells",
  {
    expect_equal(
      sprinkle_font(x = x,
                    italic = TRUE)[["body"]][["italic"]],
      rep(TRUE, 66)
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Correctly change the font_size column of the table part for the selected cells",
  {
    expect_equal(
      sprinkle_font(x = x,
                    font_size = 10)[["body"]][["font_size"]],
      rep("10", 66)
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Correctly change the font_size_units column of the table part for the selected cells",
  {
    expect_equal(
      sprinkle_font(x = x,
                    font_size_units = "em")[["body"]][["font_size_units"]],
      rep("em", 66)
    )
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "Correctly change the font_color column of the table part for the selected cells",
  {
    expect_equal(
      sprinkle_font(x = x,
                    font_color = "blue")[["body"]][["font_color"]],
      rep("blue", 66)
    )
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "Correctly change the font_family column of the table part for the selected cells",
  {
    expect_equal(
      sprinkle_font(x = x,
                    font_family = "Courier New")[["body"]][["font_family"]],
      rep("Courier New", 66)
    )
  }
)

# Functional Requirement 7 ------------------------------------------

test_that(
  "Cast an error if x is not a dust object",
  {
    expect_error(
      sprinkle_font(x = mtcars)
    )
  }
)

# Functional Requirement 8 ------------------------------------------

test_that(
  "Cast an error if bold is not logical(1)",
  {
    expect_error(
      sprinkle_font(x = x,
                    bold = "TRUE")
    )
  }
)

test_that(
  "Cast an error if bold is not logical(1)",
  {
    expect_error(
      sprinkle_font(x = x,
                    bold = c(TRUE, FALSE))
    )
  }
)

# Functional Requirement 9 ------------------------------------------

test_that(
  "Cast an error if italic is not logical(1)",
  {
    expect_error(
      sprinkle_font(x = x,
                    italic = "TRUE")
    )
  }
)

test_that(
  "Cast an error if italic is not logical(1)",
  {
    expect_error(
      sprinkle_font(x = x,
                    italic = c(TRUE, FALSE))
    )
  }
)

# Functional Requirement 10 -----------------------------------------

test_that(
  "Cast an error if font_size is not numeric(1)",
  {
    expect_error(
      sprinkle_font(x = x,
                    font_size = "3")
    )
  }
)

test_that(
  "Cast an error if font_size_units is not numeric(1)",
  {
    expect_error(
      sprinkle_font(x = x,
                    font_size = c(3, 4))
    )
  }
)

# Functional Requirement 11 -----------------------------------------

test_that(
  "Cast an error if font_size_units is not character(1)",
  {
    expect_error(
      sprinkle_font(x = x,
                    font_size_units = TRUE)
    )
  }
)

test_that(
  "Cast an error if font_size_units is not character(1)",
  {
    expect_error(
      sprinkle_font(x = x,
                    font_size_units = c("px", "pt"))
    )
  }
)

# Functional Requirement 12 -----------------------------------------

test_that(
  "Cast an error if font_size_units is not one of px, pt, em, %",
  {
    expect_error(
      sprinkle_font(x = x,
                    font_size_units = "in")
    )
  }
)

# Functional Requirement 13 -----------------------------------------

test_that(
  "Cast an error if font_color is not character(1)",
  {
    expect_error(
      sprinkle_font(x = x,
                    font_color = TRUE)
    )
  }
)

test_that(
  "Cast an error if font_color is not character(1)",
  {
    expect_error(
      sprinkle_font(x = x,
                    font_color = c("red", "blue"))
    )
  }
)

# Functional Requirement 14 -----------------------------------------

test_that(
  "Cast an error if any value in font_color is not a valid color",
  {
    expect_error(
      sprinkle_font(x = x,
                    font_color = "not a color")
    )
  }
)

# Functional Requirement 15 -----------------------------------------

test_that(
  "Cast an error if font_family is not character(1)",
  {
    expect_error(
      sprinkle_font(x = x,
                    font_family = TRUE)
    )
  }
)

test_that(
  "Cast an error if font_family is not character(1)",
  {
    expect_error(
      sprinkle_font(x = x,
                    font_family = c("red", "blue"))
    )
  }
)

# Functional Requirement 16 -----------------------------------------

test_that(
  "Cast an error if part is not one of body, head, foot, interfoot",
  {
    expect_error(
      sprinkle_font(x = x,
                    part = "not a part")
    )
  }
)

# Functional Requirement 17 -----------------------------------------

test_that(
  "Casts an error if recycle = 'none' and bold does not have length 1.",
  {
    expect_error(sprinkle_font(x, bold = c(TRUE, FALSE), recycle = "none"))
  }
)

test_that(
  "Passes when recycle != 'none' and bold does has length > 1.",
  {
    expect_silent(sprinkle_font(x, bold = c(TRUE, FALSE), recycle = "rows"))
  }
)

# Functional Requirement 18 -----------------------------------------

test_that(
  "Casts an error if recycle = 'none' and italic does not have length 1.",
  {
    expect_error(sprinkle_font(x, italic = c(TRUE, FALSE), recycle = "none"))
  }
)

test_that(
  "Passes when recycle != 'none' and italic does has length > 1.",
  {
    expect_silent(sprinkle_font(x, italic = c(TRUE, FALSE), recycle = "rows"))
  }
)

# Functional Requirement 19 -----------------------------------------

test_that(
  "Casts an error if recycle = 'none' and font_size does not have length 1.",
  {
    expect_error(sprinkle_font(x, font_size = c(10, 12), recycle = "none"))
  }
)

test_that(
  "Passes when recycle != 'none' and font_size does has length > 1.",
  {
    expect_silent(sprinkle_font(x, font_size = c(10, 12), recycle = "rows"))
  }
)

# Functional Requirement 20 -----------------------------------------

test_that(
  "Casts an error if recycle = 'none' and font_size_units does not have length 1.",
  {
    expect_error(sprinkle_font(x, font_size_units = c("px", "pt"), recycle = "none"))
  }
)

test_that(
  "Passes when recycle != 'none' and font_size_units does has length > 1.",
  {
    expect_silent(sprinkle_font(x, font_size_units = c("px", "pt"), recycle = "rows"))
  }
)

# Functional Requirement 21 -----------------------------------------

test_that(
  "Casts an error if recycle = 'none' and font_color does not have length 1.",
  {
    expect_error(sprinkle_font(x, font_color = c("red", "blue"), recycle = "none"))
  }
)

test_that(
  "Passes when recycle != 'none' and font_color does has length > 1.",
  {
    expect_silent(sprinkle_font(x, font_color = c("red", "blue"), recycle = "rows"))
  }
)

# Functional Requirement 22 -----------------------------------------

test_that(
  "Casts an error if recycle = 'none' and font_family does not have length 1.",
  {
    expect_error(sprinkle_font(x, font_family = c("Roman", "Arial"), recycle = "none"))
  }
)

test_that(
  "Passes when recycle != 'none' and font_family does has length > 1.",
  {
    expect_silent(sprinkle_font(x, font_family = c("Roman", "Arial"), recycle = "rows"))
  }
)

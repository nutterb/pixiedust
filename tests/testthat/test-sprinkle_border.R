context("sprinkle_border")

x <- dust(head(mtcars))

# Functional Requirement 1 ------------------------------------------

test_that(
  "Correctly reassigns the left_border, right_border, top_border, and 
   bottom_border columns in the table part",
  {
    expect_equal(
      sprinkle_border(x, border = "all", 
                      border_color = "green")$body[c("bottom_border", "left_border", "top_border", "right_border")],
      data.frame(bottom_border = rep("1pt solid green", 66),
                 left_border = rep("1pt solid green", 66),
                 top_border = rep("1pt solid green", 66),
                 right_border = rep("1pt solid green", 66),
                 stringsAsFactors = FALSE)
    )
  }
)

test_that(
  "Correctly reassigns the left_border, right_border, top_border, and 
   bottom_border columns in the table part",
  {
    expect_equal(
      sprinkle_border(x, border = "bottom", color = "black")$body$bottom_border,
      rep("1pt solid black", 66)
    )
  }
)

test_that(
  "Correctly reassigns the left_border, right_border, top_border, and 
   bottom_border columns in the table part",
  {
    expect_equal(
      sprinkle_border(x, border = "left", color = "black")$body$left_border,
      rep("1pt solid black", 66)
    )
  }
)

test_that(
  "Correctly reassigns the left_border, right_border, top_border, and 
   bottom_border columns in the table part",
  {
    expect_equal(
      sprinkle_border(x, border = "top", color = "black")$body$top_border,
      rep("1pt solid black", 66)
    )
  }
)

test_that(
  "Correctly reassigns the left_border, right_border, top_border, and 
   bottom_border columns in the table part",
  {
    expect_equal(
      sprinkle_border(x, border = "right", color = "black")$body$right_border,
      rep("1pt solid black", 66)
    )
  }
)

test_that(
  "Function succeeds when called on a dust_list object",
  {
    expect_silent(
      mtcars %>% 
        dplyr::group_by(am, vs) %>% 
        dust(ungroup = FALSE) %>% 
        sprinkle_border(border = "right", color = "red")
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Cast an error if x is not a dust object",
  {
    expect_error(sprinkle_border(mtcars,
                                 border = "bottom"))
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Cast an error if any element of border is not one of all, bottom, 
   left, top, or right",
  {
    expect_error(
      sprinkle_border(x, border = "middle")
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Cast an error if border_color is not a character(1)",
  {
    expect_error(
      sprinkle_border(x, border_color = 13)
    )
  }
)

test_that(
  "Cast an error if border_color is not a character(1)",
  {
    expect_error(
      sprinkle_border(x, border_color = c("black", "red"))
    )
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "Cast an error if border_color is not a valid color format",
  {
    expect_error(
      sprinkle_border(x, border_color = "not_a_color")
    )
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "Cast an error if border_style is not one of solid, dashed, dotted,
  double groove, ridge, inset, outset, hidden, none",
  {
    expect_error(
      sprinkle_border(x, border_style = "not_a_style")
    )
  }
)

test_that(
  "Accept solid, dashed, dotted, double, groove, ridge, inset, outset, hidden
   and none",
  {
    expect_silent(
      sprinkle_border(x, border_style = "solid")
    )
  }
)

test_that(
  "Accept solid, dashed, dotted, double, groove, ridge, inset, outset, hidden
   and none",
  {
    expect_silent(
      sprinkle_border(x, border_style = "dashed")
    )
  }
)

test_that(
  "Accept solid, dashed, dotted, double, groove, ridge, inset, outset, hidden
   and none",
  {
    expect_silent(
      sprinkle_border(x, border_style = "dotted")
    )
  }
)


test_that(
  "Accept solid, dashed, dotted, double, groove, ridge, inset, outset, hidden
   and none",
  {
    expect_silent(
      sprinkle_border(x, border_style = "double")
    )
  }
)

test_that(
  "Accept solid, dashed, dotted, double, groove, ridge, inset, outset, hidden
   and none",
  {
    expect_silent(
      sprinkle_border(x, border_style = "groove")
    )
  }
)

test_that(
  "Accept solid, dashed, dotted, double, groove, ridge, inset, outset, hidden
   and none",
  {
    expect_silent(
      sprinkle_border(x, border_style = "ridge")
    )
  }
)

test_that(
  "Accept solid, dashed, dotted, double, groove, ridge, inset, outset, hidden
   and none",
  {
    expect_silent(
      sprinkle_border(x, border_style = "inset")
    )
  }
)

test_that(
  "Accept solid, dashed, dotted, double, groove, ridge, inset, outset, hidden
   and none",
  {
    expect_silent(
      sprinkle_border(x, border_style = "outset")
    )
  }
)

test_that(
  "Accept solid, dashed, dotted, double, groove, ridge, inset, outset, hidden
   and none",
  {
    expect_silent(
      sprinkle_border(x, border_style = "hidden")
    )
  }
)

test_that(
  "Accept solid, dashed, dotted, double, groove, ridge, inset, outset, hidden
   and none",
  {
    expect_silent(
      sprinkle_border(x, border_style = "none")
    )
  }
)

# Functional Requirement 7 ------------------------------------------

test_that(
  "Cast an error if border_thickness is not a numeric(1)",
  {
    expect_error(
      sprinkle_border(x, border_thickness = "one")
    )
  }
)

test_that(
  "Cast an error if border_thickness is not a numeric(1)",
  {
    expect_error(
      sprinkle_border(x, border_thickness = c(1, 2))
    )
  }
)

# Functional Requirement 8 ------------------------------------------

test_that(
  "Cast an error if border_units is not one of pt or px",
  {
    expect_error(
      sprinkle_border(x, border_units = "in")
    )
  }
)

test_that(
  "border_units accepts pt",
  {
    expect_silent(
      sprinkle_border(x, border_units = "pt")
    )
  }
)

test_that(
  "border_units accepts px",
  {
    expect_silent(
      sprinkle_border(x, border_units = "px")
    )
  }
)

# Functional Requirement 9 - 11 ------------------------------------

# Tested in index_to_sprinkle

# Functional Requirement 12 -----------------------------------------

test_that(
  "Casts an error if recycle = 'none' and border_color does not have length 1.",
  {
    expect_error(sprinkle_border(x, border_color = c("red", "blue"), recycle = "none"))
  }
)

test_that(
  "Passes when recycle != 'none' and border_color does has length > 1.",
  {
    expect_silent(sprinkle_border(x, border_color = c("red", "blue"), recycle = "rows"))
  }
)

# Functional Requirement 13 -----------------------------------------

test_that(
  "Casts an error if recycle = 'none' and border_style does not have length 1.",
  {
    expect_error(sprinkle_border(x, border_style = c("dashed", "solid"), recycle = "none"))
  }
)

test_that(
  "Passes when recycle != 'none' and border_color does has length > 1.",
  {
    expect_silent(sprinkle_border(x, border_style = c("dashed", "solid"), recycle = "rows"))
  }
)

# Functional Requirement 14 -----------------------------------------

test_that(
  "Casts an error if recycle = 'none' and border_thickness does not have length 1.",
  {
    expect_error(sprinkle_border(x, border_thickness = c(1, 3), recycle = "none"))
  }
)

test_that(
  "Passes when recycle != 'none' and border_thickness does has length > 1.",
  {
    expect_silent(sprinkle_border(x, border_thickness = c(1, 3), recycle = "rows"))
  }
)

# Functional Requirement 15 -----------------------------------------

test_that(
  "Quietly restrict border_units to just the first element if it has length > 1 and recycle = 'none'",
  {
    expect_silent(sprinkle_border(x, border_units = c("px", "pt"), recycle = "none"))
  }
)

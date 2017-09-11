context("sprinkle_align")

x <- dust(head(mtcars))

# Functional Requirement 1 ------------------------------------------

test_that(
  "Correctly reassigns the appropriate elements of the halign column in the table part.",
  {
    halign <- vector("character", nrow(x$body))
    halign[x$body$row == 1] <- "left"
    expect_equal(
      sprinkle_align(x, rows = 1, halign = "left")$body$halign,
      halign
    )
  }
)

test_that(
  "Correctly reassigns the appropriate elements of the halign column in the table part.",
  {
    halign <- vector("character", nrow(x$body))
    halign[x$body$row == 2 & x$body$col %in% 4:5] <- "center"
    expect_equal(
      sprinkle_align(x, rows = 2, cols = 4:5, halign = "center")$body$halign,
      halign
    )
  }
)

test_that(
  "Correctly reassigns the appropriate elements bg column in the table part.",
  {
    halign <- vector("character", nrow(x$body))
    halign[x$body$row == 2 & x$body$col %in% 4:5] <- "right"
    expect_equal(
      sprinkle_align(x, rows = c(2, 2), cols = 4:5, halign = "right",
                  fixed = TRUE)$body$halign,
      halign
    )
  }
)

test_that(
  "Correctly reassigns the appropriate elements of the valign column in the table part.",
  {
    valign <- vector("character", nrow(x$body))
    valign[x$body$row == 1] <- "top"
    expect_equal(
      sprinkle_align(x, rows = 1, valign = "top")$body$valign,
      valign
    )
  }
)

test_that(
  "Correctly reassigns the appropriate elements of the valign column in the table part.",
  {
    valign <- vector("character", nrow(x$body))
    valign[x$body$row == 2 & x$body$col %in% 4:5] <- "middle"
    expect_equal(
      sprinkle_align(x, rows = 2, cols = 4:5, valign = "middle")$body$valign,
      valign
    )
  }
)

test_that(
  "Correctly reassigns the appropriate elements bg column in the table part.",
  {
    valign <- vector("character", nrow(x$body))
    valign[x$body$row == 2 & x$body$col %in% 4:5] <- "bottom"
    expect_equal(
      sprinkle_align(x, rows = c(2, 2), cols = 4:5, valign = "bottom",
                     fixed = TRUE)$body$valign,
      valign
    )
  }
)

test_that(
  "Correctly skips reassignment when valign = halign = NULL",
  {
    expect_equal(
      sprinkle_align(x, valign = NULL, halign = NULL),
      x
    )
  }
)

test_that(
  "Function succeeds when called on a dust_list object",
  {
    expect_silent(
      dplyr::group_by(mtcars, am, vs) %>% 
        dust(ungroup = FALSE) %>% 
        sprinkle_align(halign = "left")
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Casts an error if x is not a dust object.",
  {
    expect_error(sprinkle_align(mtcars))
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Casts an error if bg is not a character(1)",
  {
    expect_error(sprinkle_align(x, halign = c("left", "center")))
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Casts an error if part is not one of body, head, foot, or interfoot",
  {
    expect_error(sprinkle_align(x, halign = "left", part = "not_a_part"))
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "Casts an error if fixed is not a logical(1)",
  {
    expect_error(sprinkle_align(x, valign = "top", fixed = "yes"))
  }
)

test_that(
  "Casts an error if fixed is not a logical(1)",
  {
    expect_error(sprinkle_align(x, valign = "bottom", fixed = c(TRUE, FALSE)))
  }
)

test_that(
  "Casts an error if recycle is not one of none, rows, or cols",
  {
    expect_error(sprinkle_align(x, halign = "right", recycle = "not_an_option"))
  }
)
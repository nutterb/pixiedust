context("sprinkle_merge")

x <- dust(head(mtcars))

# Functional Requirement 1 ------------------------------------------

test_that(
  "Correctly reassigns the appropriate elements of merge, merge_rowval and 
   merge_colval columns in the table part.",
  {
    merge <- vector("logical", nrow(x$body))
    merge[c(1, 2, 7, 8)] <- TRUE
    expect_equal(
      sprinkle_merge(x, rows = 1:2, cols = 1:2, merge = TRUE)$body$merge,
      merge
    )
  }
)

test_that(
  "Correctly reassigns the appropriate elements of merge, merge_rowval and 
   merge_colval columns in the table part.",
  {
    expect_equal(
      sprinkle_merge(x, rows = 1:2, cols = 1:2, merge = TRUE,
                     merge_rowval = 2)$body$merge_rowval[c(1, 2, 7, 8)],
      c(1, 2, 1, 2)
    )
  }
)

test_that(
  "Correctly reassigns the appropriate elements of merge, merge_rowval and 
   merge_colval columns in the table part.",
  {
    expect_equal(
      sprinkle_merge(x, rows = 1:2, cols = 1:2, merge = TRUE,
                     merge_rowval = 2,
                     merge_colval = 2)$body$merge_colval[c(1, 2, 7, 8)],
      c(1, 1, 2, 2)
    )
  }
)

test_that(
  "Works with a dust_list object",
  {
    expect_silent(
      dplyr::group_by(mtcars, am, vs) %>% 
        dust(ungroup = FALSE) %>% 
        sprinkle_merge(rows = 1, cols = 1:2, merge = TRUE)
    )
  }
)


test_that(
  "Correctly skips merging when merge = FALSE",
  {
    expect_equal(
      sprinkle_merge(x, rows = 1:2, cols = 1:2, merge = FALSE),
      x
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Casts an error if x is not a dust object.",
  {
    expect_error(sprinkle_merge(mtcars))
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Casts an error if merge is not a logical(1)",
  {
    expect_error(sprinkle_merge(x, merge = c(TRUE, FALSE)))
  }
)

test_that(
  "Casts an error if merge is not a logical(1)",
  {
    expect_error(sprinkle_merge(x, merge = c("TRUE")))
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Casts an error if merge_rowval is not a numeric(1)",
  {
    expect_error(suppressMessages(sprinkle_merge(x, merge_rowval = 1:2)))
  }
)

test_that(
  "Casts an error if merge is not a logical(1)",
  {
    expect_error(suppressMessages(sprinkle_merge(x, merge_rowval = "one")))
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "Casts an error if merge_colval is not a numeric(1)",
  {
    expect_error(suppressMessages(sprinkle_merge(x, merge_colval = 1:2)))
  }
)

test_that(
  "Casts an error if merge is not a logical(1)",
  {
    expect_error(suppressMessages(sprinkle_merge(x, merge_colval = "one")))
  }
)


# Functional Requirement 6 ------------------------------------------

test_that(
  "Casts an error if part is not one of body, head, foot, or interfoot",
  {
    expect_error(sprinkle_align(x, halign = "left", part = "not_a_part"))
  }
)

# Functional Requirement 7 ------------------------------------------

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
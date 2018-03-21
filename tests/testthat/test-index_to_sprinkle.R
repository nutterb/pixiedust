context("index_to_sprinkle.R")

x <- dust(head(mtcars))

# Tests to cover aspects of index_to_sprinkle note picked up by other tests

test_that(
  "`rows` must be either numeric or a call object",
  {
    expect_error(sprinkle_bg(x, 
                          rows = c("1", "2"), 
                          bg = "blue"))
  }
)


test_that(
  "`cols` must be a numeric or character vector",
  expect_error(
    sprinkle_bg(x,
                cols = c(TRUE, FALSE),
                bg = "blue")
  )
)


test_that(
  "When `fixed = TRUE`, rows and cols must have the same length",
  {
    expect_error(
      sprinkle_bg(x,
                  cols = 1:2,
                  rows = 1:3,
                  fixed = TRUE,
                  bg = "blue")
    )
  }
)


test_that(
  "Cast an error for invalid rows",
  {
    expect_error(
      sprinkle_bg(x, rows = 7, bg = "blue")
    )
  }
)


test_that(
  "Cast an error for invalid columns",
  {
    expect_error(
      sprinkle_bg(x, rows = 12, bg = "blue")
    )
  }
)


test_that(
  "recycle = columns is correctly translated",
  {
    expect_silent(
      sprinkle_bg(x, rows = 1:2, cols = 1:3, 
                  bg = c("red", "blue"),
                  recycle = "columns")
    )
  }
)


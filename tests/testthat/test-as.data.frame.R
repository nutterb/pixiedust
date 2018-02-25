context("as.data.frame.dust")

# Functional Requirement 1 ------------------------------------------

test_that(
  "FR 1: Accepts an object of class `dust`",
  {
    skip_on_cran()
    expect_silent(
      as.data.frame.dust(dust(mtcars))
    )
  }
)

test_that(
  "FR 1: Accepts an object of class `dust_list`",
  {
    skip_on_cran()
    expect_silent(
      split(mtcars, mtcars$am) %>%
        dust() %>%
        as.data.frame()
    )
  }
)

test_that(
  "FR 1: Casts an error when passed an object not of class `dust` or `dust_list`",
  {
    skip_on_cran()
    expect_error(
      pixiedust:::as.data.frame.dust(mtcars)
    )
  }
)

test_that(
  "FR 1: Casts an error when passed an object not of class `dust` or `dust_list`",
  {
    skip_on_cran()
    expect_error(
      pixiedust:::as.data.frame.dust_list(mtcars)
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "FR 2: Accepts a logical indicating if sprinkles should be applied.",
  {
    skip_on_cran()
    expect_silent(
      dust(mtcars) %>%
        as.data.frame(sprinkled = TRUE)
    )
  }
)

test_that(
  "FR 2: Accepts a logical indicating if sprinkles should be applied (dust_list).",
  {
    skip_on_cran()
    expect_silent(
      split(mtcars, mtcars$am) %>%
        dust() %>%
        as.data.frame(sprinkled = TRUE)
    )
  }
)

test_that(
  "FR 2: Casts error when `sprinkled` is not logical",
  {
    skip_on_cran()
    expect_error(
      dust(mtcars) %>%
        as.data.frame(sprinkled = 1)
    )
  }
)

test_that(
  "FR 2: Casts error when `sprinkled` is not logical (dust_list)",
  {
    skip_on_cran()
    expect_error(
      split(mtcars, mtcars$am) %>%
        dust() %>%
        as.data.frame(sprinkled = 1)
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "FR 3: Return a data frame object",
  {
    skip_on_cran()
    fit <- lm(mpg ~ qsec + factor(am) + wt * factor(gear), data = mtcars)
    Dust <- dust(fit) %>%
      sprinkle(cols = 2:4, round = 2) %>%
      sprinkle(cols = 5, fn = quote(pvalString(value))) %>%
      sprinkle(cols = 3, font_color = "#DA70D6") %>%
      sprinkle_print_method("html")
    expect_true("data.frame" %in% class(as.data.frame(Dust)))
  }
)

test_that(
  "FR 3: Return a data frame object when unsprinkled",
  {
    skip_on_cran()
    fit <- lm(mpg ~ qsec + factor(am) + wt * factor(gear), data = mtcars)
    Dust <- dust(fit) %>%
      sprinkle(cols = 2:4, round = 2) %>%
      sprinkle(cols = 5, fn = quote(pvalString(value))) %>%
      sprinkle(cols = 3, font_color = "#DA70D6") %>%
      sprinkle_print_method("html")
    expect_true("data.frame" %in% class(as.data.frame(Dust, sprinkled = FALSE)))
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "FR 4: Return a list of data frames (dust_list)",
  {
    skip_on_cran()
    Dust <- split(mtcars, mtcars$am) %>%
      dust() %>%
      as.data.frame()
    
    dust_class <- vapply(Dust, 
                         function(x) "data.frame" %in% class(x),
                         logical(1))
    
    expect_true(all(dust_class))
  }
)

test_that(
  "FR 4: Return a list of data frames when unsprinkled (dust_list)",
  {
    skip_on_cran()
    Dust <- split(mtcars, mtcars$am) %>%
      dust() %>%
      as.data.frame(sprinkled = FALSE)
    
    dust_class <- vapply(Dust, 
                         function(x) "data.frame" %in% class(x),
                         logical(1))
    
    expect_true(all(dust_class))
  }
)

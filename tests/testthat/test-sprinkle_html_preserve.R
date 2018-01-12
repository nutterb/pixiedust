context("sprinkle_html_preserve")

x <- dust(mtcars)

# Functional Requirement 1 ------------------------------------------

test_that(
  "Change the html_preserve attribute of the dust object",
  {
    expect_equal(
      sprinkle_html_preserve(x, TRUE)[["html_preserve"]],
      TRUE
    )
  }
)

test_that(
  "Change the html_preserve attribute of the dust object",
  {
    expect_equal(
      sprinkle_html_preserve(x, FALSE)[["html_preserve"]],
      FALSE
    )
  }
)

test_that(
  "Function succeeds when called on a dust_list object",
  {
    expect_silent(
      dplyr::group_by(mtcars, am, vs) %>% 
        dust(ungroup = FALSE) %>% 
        sprinkle_html_preserve(html_preserve = TRUE)
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Cast an error if x is not a dust object",
  {
    expect_error(
      sprinkle_html_preserve(mtcars)
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Cast an error if x is not a logical object",
  {
    expect_error(
      sprinkle_html_preserve(x, "FALSE")
    )
  }
)

test_that(
  "Cast an error if html_preserve is not logical(1)",
  {
    expect_error(
      sprinkle_html_preserve(x, c(TRUE, FALSE))
    )
  }
)

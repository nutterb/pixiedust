context("print_dust_html")

test_that(
  "print_dust_html with fixed header",
  {
    skip_on_cran()
    x <- dust(mtcars) %>% 
      sprinkle(fixed_header = TRUE) 
      
    expect_silent(print_dust_html(x))
  }
)

test_that(
  "print_dust_html with interactive = FALSE",
  {
    skip_on_cran()
    x <- dust(mtcars)
    
    expect_silent(print_dust_html(x,
                                  interactive = FALSE))
  }
)

test_that(
  "print_dust_html with interactive = FALSE, asis = FALSE",
  {
    skip_on_cran()
    x <- dust(mtcars)
    
    expect_silent(print_dust_html(x, 
                                  interactive = FALSE, 
                                  asis = FALSE))
  }
)


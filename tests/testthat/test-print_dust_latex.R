context("print_dust_latex")

test_that(
  "print_dust_latex",
  {
    skip_on_cran()
    fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
    x <- dust(fit) %>% 
      sprinkle(rows = 2:4,
               cols = 2:4,
               bg = "black",
               bold = TRUE,
               border_collapse = "collapse",
               border = c("left", "right"),
               border_thickness = 2,
               border_units = "px",
               border_style = "solid",
               border_color = "purple",
               halign = "left",
               height = 7,
               height_units = "px",
               fn = quote(value * -1),
               font_color = "orchid",
               font_size = 14,
               font_size_units = "px",
               italic = TRUE,
               pad = 8,
               round = 3,
               rotate_degree = -45,
               valign = "bottom",
               width = 15,
               width_units = "%") %>%
      sprinkle_print_method("latex")
    
    expect_silent(print_dust_latex(x))
  })

test_that(
  "print_dust_latex with a label",
  {
    skip_on_cran()
    fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
    x <- dust(fit,
              label = "some label") %>%
      sprinkle_print_method("latex")
    
    expect_silent(print_dust_latex(x))
  })

test_that(
  "print_dust_latex with bookdown and no label",
  {
    skip_on_cran()
    fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
    x <- dust(fit,
              bookdown = TRUE) %>%
      sprinkle_print_method("latex")
    
    expect_silent(print_dust_latex(x))
  })

test_that(
  "print_dust_latex with bookdown and a label",
  {
    skip_on_cran()
    fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
    x <- dust(fit,
              bookdown = TRUE,
              label = "some label") %>%
      sprinkle_print_method("latex")
    
    expect_silent(print_dust_latex(x))
  })

test_that(
  "print_dust_latex with longtable",
  {
    skip_on_cran()
    fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
    x <- dust(fit,
              longtable = TRUE) %>%
      sprinkle_print_method("latex")
    
    expect_silent(print_dust_latex(x))
  })

test_that(
  "print_dust_latex with float = FALSE",
  {
    skip_on_cran()
    fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
    x <- dust(fit,
              float = FALSE) %>%
      sprinkle_print_method("latex")
    
    expect_silent(print_dust_latex(x))
  })

test_that(
  "print_dust_latex with HTML colors",
  {
    skip_on_cran()
    fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
    x <- dust(fit,
              float = FALSE) %>%
      sprinkle(bg = "#FFFFFF") %>% 
      sprinkle_print_method("latex")
    
    expect_silent(print_dust_latex(x))
  })

test_that(
  "print_dust_latex with rgb color",
  {
    skip_on_cran()
    fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
    x <- dust(fit,
              float = FALSE) %>%
      sprinkle(bg = "rgb(255,255,255)") %>% 
      sprinkle_print_method("latex")
    
    expect_silent(print_dust_latex(x))
  })


test_that(
  "print_dust_latex with vertical dashed border",
  {
    skip_on_cran()
    fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
    x <- dust(fit,
              float = FALSE) %>%
      sprinkle(cols = 2,
               border = "right",
               border_style = "dashed") %>% 
      sprinkle_print_method("latex")
    
    expect_silent(print_dust_latex(x))
  })

test_that(
  "print_dust_latex with horizontal borders",
  {
    skip_on_cran()
    fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
    x <- dust(fit,
              float = FALSE) %>%
      sprinkle(border = "bottom") %>% 
      sprinkle_print_method("latex")
    
    expect_silent(print_dust_latex(x))
  })

test_that(
  "print_dust_latex with horizontal borders",
  {
    skip_on_cran()
    fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
    x <- dust(fit,
              float = FALSE) %>%
      sprinkle(border = "bottom",
               border_style = "dashed") %>% 
      sprinkle_print_method("latex")
    
    expect_silent(print_dust_latex(x))
  })

test_that(
  "print_dust_latex sanitization",
  {
    skip_on_cran()
    fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
    x <- dust(fit,
              float = FALSE) %>%
      sprinkle(sanitize = TRUE) %>% 
      sprinkle_print_method("latex")
    
    expect_silent(print_dust_latex(x))
  })
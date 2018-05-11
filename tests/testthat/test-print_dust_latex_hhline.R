context("print_dust_latex_hhline")

test_that(
  "print_dust_latex_hhline",
  {
    skip_on_cran()
    fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
    x <- dust(fit,
              hhline = TRUE) %>% 
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
    
    expect_silent(print_dust_latex_hhline(x))
  })

test_that(
  "print_dust_latex_hhline with a label",
  {
    skip_on_cran()
    fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
    x <- dust(fit,
              hhline = TRUE,
              label = "some label") %>%
      sprinkle_print_method("latex")
    
    expect_silent(print_dust_latex_hhline(x))
  })

test_that(
  "print_dust_latex_hhline with bookdown and no label",
  {
    skip_on_cran()
    fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
    x <- dust(fit,
              hhline = TRUE,
              bookdown = TRUE) %>%
      sprinkle_print_method("latex")
    
    expect_silent(print_dust_latex_hhline(x))
  })

test_that(
  "print_dust_latex_hhline with bookdown and a label",
  {
    skip_on_cran()
    fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
    x <- dust(fit,
              hhline = TRUE,
              bookdown = TRUE,
              label = "some label") %>%
      sprinkle_print_method("latex")
    
    expect_silent(print_dust_latex_hhline(x))
  })

test_that(
  "print_dust_latex_hhline with longtable",
  {
    skip_on_cran()
    fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
    x <- dust(fit,
              hhline = TRUE,
              longtable = TRUE) %>%
      sprinkle_print_method("latex")
    
    expect_silent(print_dust_latex_hhline(x))
  })

test_that(
  "print_dust_latex_hhline with float = FALSE",
  {
    skip_on_cran()
    fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
    x <- dust(fit,
              hhline = TRUE,
              float = FALSE) %>%
      sprinkle_print_method("latex")
    
    expect_silent(print_dust_latex_hhline(x))
  })

test_that(
  "print_dust_latex with hhline and horizontal borders",
  {
    skip_on_cran()
    fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
    x <- dust(fit,
              hhline = TRUE,
              float = FALSE) %>%
      sprinkle(border = "bottom") %>% 
      sprinkle_print_method("latex")
    
    expect_silent(print_dust_latex_hhline(x))
  })

test_that(
  "print_dust_latex with hhline and horizontal borders",
  {
    skip_on_cran()
    fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
    x <- dust(fit,
              hhline = TRUE,
              float = FALSE) %>%
      sprinkle(border = "bottom",
               border_style = "hidden") %>% 
      sprinkle_print_method("latex")
    
    expect_silent(print_dust_latex_hhline(x))
  })

test_that(
  "print_dust_latex with hhline and horizontal borders",
  {
    skip_on_cran()
    fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
    x <- dust(fit,
              hhline = TRUE,
              float = FALSE) %>%
      sprinkle(border = "bottom",
               border_style = "solid") %>% 
      sprinkle_print_method("latex")
    
    expect_silent(print_dust_latex_hhline(x))
  })

test_that(
  "print_dust_latex with hhline and horizontal borders",
  {
    skip_on_cran()
    fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
    x <- dust(fit,
              hhline = TRUE,
              float = FALSE) %>%
      sprinkle(border = "bottom",
               border_style = "double") %>% 
      sprinkle_print_method("latex")
    
    expect_silent(print_dust_latex_hhline(x))
  })
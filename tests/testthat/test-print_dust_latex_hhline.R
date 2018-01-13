context("print_dust_latex_hhline")

test_that(
  "print_dust_latex_hhline",
  {
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
    fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
    x <- dust(fit,
              hhline = TRUE,
              float = FALSE) %>%
      sprinkle(border = "bottom") %>% 
      sprinkle_print_method("latex")
    
    expect_silent(print_dust_latex(x))
  })
context("print.dust")

test_that("printing to console succeeds with defaults",
{
  fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
  x <- dust(fit)
  
  expect_silent(x)
})

test_that("printing to console succeeds with sprinkles",
{
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
             width_units = "%")
  
  expect_silent(x)
})

test_that("printing to console succeeds with sprinkles",
{
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
    sprinkle_print_method("markdown")
          
  expect_silent(x)
})

test_that("printing to console succeeds with sprinkles",
{
  fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
  x <- dust(fit) %>% 
    sprinkle(rows = 2:4,
             cols = 2:4,
             bg = "black",
             bold = TRUE,
             border_collapse = "inherit",
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
    sprinkle_print_method("html")
          
  expect_silent(x)
})

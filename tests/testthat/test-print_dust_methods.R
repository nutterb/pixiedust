context("print_dust_methods")

test_that("print_dust_console",
{
  fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
  x <- dust(fit) %>% 
    sprinkle(rows = 2:4,
             cols = 2:4,
             bg = "black",
             bold = TRUE,
             border_collapse = FALSE,
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
  
  expect_that(print_dust_console(x), not(throws_error()))
})

test_that("print_dust_html",
{
  fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
  x <- dust(fit) %>% 
    sprinkle(rows = 2:4,
             cols = 2:4,
             bg = "black",
             bold = TRUE,
             border_collapse = FALSE,
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
            
  expect_that(print_dust_html(x), not(throws_error()))
})

test_that("print_dust_markdown",
{
  fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
  x <- dust(fit) %>% 
    sprinkle(rows = 2:4,
             cols = 2:4,
             bg = "black",
             bold = TRUE,
             border_collapse = FALSE,
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
  
  prints_text(print_dust_markdown(x))
})
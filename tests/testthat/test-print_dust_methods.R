context("print_dust_methods")

test_that("print_dust_console",
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
             width_units = "%")
  
  expect_output(print_dust_console(x), "")
})

test_that("print_dust_html",
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
    sprinkle_print_method("html")
            
  expect_silent(print_dust_html(x))
})

test_that("print_dust_html: correction for multiple cell merge",
{
  skip_on_cran()
  custom_head <- rbind(names(mtcars), 
                       labelVector::get_label(mtcars,
                                              names(mtcars))) %>%
    as.data.frame(stringsAsFactors = FALSE)
  
  custom_foot <- rbind(vapply(mtcars, mean, numeric(1)),
                       vapply(mtcars, sd, numeric(1))) %>%
    as.data.frame(stringsAsFactors = FALSE)
  
  custom_interfoot <- data.frame("To Be Continued", 
                                 "", "", "", "", "", "",
                                 "", "", "", "")
  suppressWarnings(
  x <- dust(mtcars) %>%
     redust(custom_head, part = "head") %>%
     redust(custom_foot, part = "foot") %>%
     redust(custom_interfoot, part = "interfoot") %>%
     sprinkle_table(round = 2, longtable = 4) %>%
     sprinkle(bg = "gray", part = "head") %>%
     sprinkle(bg = "lightgray", part = "foot") %>%
     sprinkle(bg = "lightgray", part = "interfoot") %>%
     sprinkle(merge = TRUE, halign = "center", part = "interfoot") %>%
     sprinkle_print_method("html")
  )
  
  expect_silent(print_dust_html(x))
})

test_that("print_dust_markdown",
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
    sprinkle_print_method("markdown")
  
  expect_silent(print_dust_markdown(x))
})


test_that("print_dust_latex",
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
  "missing values in LaTeX output - sanitization",
  {
    skip_on_cran()
    DF <- head(mtcars)
    DF$mpg[c(1, 3, 4)] <- NA
    
    expect_silent(
      dust(DF) %>% 
        sprinkle(sanitize = TRUE) %>% 
        sprinkle_print_method("latex")
    )
  }
)

test_that(
  "print_dust_latex does not output invalid -Inf width or height", 
  {
    skip_on_cran()
    fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
    x <- print_dust_latex(dust(fit))
    expect_false(grepl("-Inf", x))
  }
)

test_that(
  "html tables with bookdown label",
  {
    skip_on_cran()
    fit <- lm(mpg ~ qsec + factor(am) + wt + factor(gear), data = mtcars)
    x <- dust(fit,
              caption = "Table heading",
              label = "table-ref",
              bookdown = TRUE) %>%
      sprinkle_print_method("html") %>%
      print(asis = FALSE)
    expect_match(x, "#tab:table-ref")
  }
)


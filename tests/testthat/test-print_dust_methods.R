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
  
  expect_output(print_dust_console(x), "")
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
            
  expect_silent(print_dust_html(x))
})

test_that("print_dust_html: correction for multiple cell merge",
{
  custom_head <- rbind(names(mtcars), Hmisc::label(mtcars)) %>%
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
  
  expect_silent(print_dust_markdown(x))
})


test_that("print_dust_latex",
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
              sprinkle_print_method("latex")
            
            expect_silent(print_dust_latex(x))
          })

test_that(
  "missing values in LaTeX output - sanitization",
  {
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
  "html tables with bookdown label",
  {
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
  
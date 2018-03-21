context("sprinkles")

x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))

test_that(
  "Cast an error if no sprinkles are given",
  {
    skip_on_cran()
    expect_error(sprinkle(x),
                 "At least one sprinkle")
  }
)

test_that(
  "Cast an error if unnamed sprinkles are given",
  {
    skip_on_cran()
    expect_error(sprinkle(x, rows = 1, cols = 1, "green"),
                 "Arguments to ... must be named")
  }
)

test_that(
  "Cast an error if a non-existent sprinkle is given",
  {
    skip_on_cran()
    expect_error(sprinkle(x, not_a_sprinkle = "abc"),
                 "could not be matched")
  }
)

test_that("sprinkles: bg",
{
  skip_on_cran()
  expect_equal(sprinkle(x, rows = 1, cols = 1, bg = "orchid")$body$bg[1],
               "orchid")
})


test_that("sprinkles: bg_pattern",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, 
                        bg_pattern = c("orchid", "aquamarine"))$body$bg[1:2],
               c("orchid", ""))
})

# test_that("sprinkles: bg_pattern_by",
# {
#   x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
#   expect_equal(sprinkle(x, rows=1, cols=1, bg_pattern_by = "columns")$bg_pattern_by,
#                "columns")
# })

test_that("sprinkles: bg_pattern_by gives error when invalid option is given",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, bg_pattern_by = "diagonal"),
               "")
})

test_that("sprinkles: bold",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, bold = TRUE)$body$bold[1],
               TRUE)
})

test_that("sprinkles: bold errors",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, bold = c(1, 2)),
               "")
})

test_that("sprinkles: border_collapse",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows=1, cols = 1, 
                        border_collapse = "separate")$border_collapse,
               "separate")
})

test_that("sprinkles: border_collapse errors",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, border_collapse = c(1, 0)),
               "")
})

test_that("sprinkles: border",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, 
                        border = c("left", "right"))$body$left_border[1],
               "1pt solid black")
})

test_that("sprinkles: border errors",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, border = "everything"),
               "")
})

test_that("sprinkles: border_thickness",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, 
                        border_thickness = 2)$body$top_border[1],
               "2pt solid black")
})

test_that("sprinkles: border_thickness errors",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, border_thickness = c(1, 2)),
               "")
})

test_that("sprinkles: border_units",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, 
                        border_units = "px")$body$top_border[1],
               "1px solid black")
})

test_that("sprinkles: border_units errors",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, border_units = c("inches")),
               "")
})

test_that("sprinkles: border_style",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, 
                        border_style = "dashed")$body$top_border[1],
               "1pt dashed black")
})

test_that("sprinkles: border_style errors",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, border_style = c("halo")),
               "")
})

test_that("sprinkles: border_color",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, 
                        border_color = "Black")$body$top_border[1],
               "1pt solid Black")
})

test_that("sprinkles: border_color errors",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, border_color = 1),
               "")
})

test_that("sprinkles: halign",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, 
                        halign = "left")$body$halign[1],
               "left")
})

test_that("sprinkles: halign errors",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, halign = c("left", "right")),
               "")
})
               
test_that("sprinkles: height",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, height = 12)$body$height[1],
               "12")
})

test_that("sprinkles: height errors",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, height = c(12, "text")),
               "")
})

test_that("sprinkles: height_units",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, 
                        height_units = "px")$body$height_units[1],
               "px")
})

test_that("sprinkles: height_units errors",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, height_units = "em"),
               "")
})

test_that("sprinkles: fn",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, 
                        fn = quote(pvalString(value)))$body$fn[1],
               "pvalString(value)")
})

test_that("sprinkles: font_color",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, 
                        font_color = "orchid")$body$font_color[1],
               "orchid")
})
           
test_that("sprinkles: font_color error",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, font_color = c("orchid", "aquamarine")),
               "")
})

test_that("sprinkles: font_size",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, 
                        font_size = 12)$body$font_size[1],
               "12")
})

test_that("sprinkles: font_size errors",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, font_size = c("1", "x")),
               "")
})

test_that("sprinkles: font_size_units",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, 
                        font_size_units = "em")$body$font_size_units[1],
               "em")
})

test_that("sprinkles: font_size_unit errors",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, font_size_units = c("inches")),
               "")
})

test_that("sprinkles: italic",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, 
                        italic = TRUE)$body$italic[1],
               TRUE)
})

test_that("sprinkles: italic errors",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, italic = c(1, 2)),
               "")
})

test_that("sprinkles: longtable accepts TRUE",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, longtable = TRUE)$longtable,
               TRUE)
})

test_that("sprinkles: longtable accepts FALSE",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, longtable = FALSE)$longtable,
               FALSE)
})

test_that("sprinkles: longtable accepts a number",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, longtable = 10)$longtable,
               10)
})

test_that("sprinkles: longtable- character resolve to FALSE",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, longtable = "character"))
})

test_that("sprinkles: merge",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_silent(sprinkle(x, rows = 3:4, cols = 1:2, merge = TRUE,
                       merge_rowval = 4, merge_colval = 2))
})

test_that("sprinkles: merge is not logical",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 3:4, cols = 1:2, merge = "yes please",
                       merge_rowval = 4, merge_colval = 2),
               "")
})

test_that("sprinkles: merge_rowval without merge casts message",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_message(sprinkle(x, rows = 3:4, cols = 1:2,
                       merge_rowval = 4),
               "")
})

test_that("sprinkles: merge_colval without merge casts message",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_message(sprinkle(x, rows = 3:4, cols = 1:2,
                       merge_colval = 4),
               "")
})

test_that("sprinkles: na_string",
{
  skip_on_cran()
  x <- dust(aov(mpg ~ factor(am) + factor(gear), data = mtcars))
  expect_equal(sprinkle(x, na_string = "")$body$na_string,
               rep("", 18))
})

test_that("sprinkles: na_string not character",
{
  skip_on_cran()
  x <- dust(aov(mpg ~ factor(am) + factor(gear), data = mtcars))
  expect_error(sprinkle(x, na_string = 1),
               "")
}) 

test_that("sprinkles: pad",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, pad = 5)$body$pad[1],
               "5")
})

test_that("sprinkles: pad errors",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, pad = c(1, "text")),
               "")
})

test_that("sprinkles: replace",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, replace = "Intercept")$body$replace[1],
              "Intercept")
})

test_that("sprinkles: replace errors",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, 
                        replace = c("Intercept", "1/4 Mile Time")),
               "")
})

test_that("sprinkles: round",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, round = 5)$body$round[1],
               "5")
})

test_that("sprinkles: round errors",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, round = c(1, "text")),
               "")
})

test_that("sprinkles: rotate_degree",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, 
                        rotate_degree = 5)$body$rotate_degree[1],
               "5")
})

test_that("sprinkles: rotate_degree errors",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, rotate_degree = c(1, "text")),
               "")
})

test_that("sprinkles: valign",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, valign = "middle")$body$valign[1],
               "middle")
          })

test_that("sprinkles: valign errors",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, valign = "above"),
               "")
})

test_that("sprinkles: width",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, width = 12)$body$width[1],
               "12")
})

test_that("sprinkles: width errors",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, width = c(12, "text")),
               "")
})

test_that("sprinkles: width_units",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, 
                        width_units = "px")$body$width_units[1],
               "px")
})

test_that("sprinkles: width_units errors",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, width_units = "em"),
               "")
})

test_that("sprinkles: no sprinkles given",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data=mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1),
               "")
})

test_that("sprinkles: single unnamed sprinkle",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, "green"),
               "")
})

test_that("sprinkles: multiple unnamed sprinkles",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, "green", "dotted"),
               "")
})

test_that("sprinkles: mixture of named and unnamed sprinkles",
{
  skip_on_cran()
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, border_color = "green", "dotted"),
               "")
})


test_that(
  "sprinkles: fixed argument",
  {
    skip_on_cran()
    x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars)) %>%
      sprinkle(cols = c(1, 2, 3),
               rows = c(1, 2, 3),
               bg = "green",
               fixed = TRUE) 
    expect_output(print(x))
  }
)


test_that(
  "sprinkles: longtable",
  {
    skip_on_cran()
    expect_silent(
      dust(mtcars) %>%
        sprinkle(longtable = 10)
    )
  }
)
          
  
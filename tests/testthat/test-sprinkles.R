context("sprinkles")

test_that("sprinkles: bg",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, bg = "orchid")$body$bg[1],
               "orchid")
})

test_that("sprinkles: bg casts an error when 'bg' has length > 1",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, bg = c("orchid", "white")),
               "")
})

test_that("sprinkles: bg_pattern",
{
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
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, bg_pattern_by = "diagonal"),
               "")
})

test_that("sprinkles: bold",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, bold = TRUE)$body$bold[1],
               TRUE)
})

test_that("sprinkles: bold errors",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, bold = c(1, 2)),
               "")
})

test_that("sprinkles: border_collapse",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows=1, cols = 1, 
                        border_collapse = FALSE)$border_collapse,
               FALSE)
})

test_that("sprinkles: border_collapse errors",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, border_collapse = c(1, 0)),
               "")
})

test_that("sprinkles: border",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, 
                        border = c("left", "right"))$body$left_border[1],
               "1px solid Black")
})

test_that("sprinkles: border errors",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, border = "everything"),
               "")
})

test_that("sprinkles: border_thickness",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, 
                        border_thickness = 2)$body$top_border[1],
               "2px solid Black")
})

test_that("sprinkles: border_thickness errors",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, border_thickness = c(1, 2)),
               "")
})

test_that("sprinkles: border_units",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, 
                        border_units = "px")$body$top_border[1],
               "1px solid Black")
})

test_that("sprinkles: border_units errors",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, border_units = c("inches")),
               "")
})

test_that("sprinkles: border_style",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, 
                        border_style = "dashed")$body$top_border[1],
               "1px dashed Black")
})

test_that("sprinkles: border_style errors",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, border_style = c("halo")),
               "")
})

test_that("sprinkles: border_color",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, 
                        border_color = "Black")$body$top_border[1],
               "1px solid Black")
})

test_that("sprinkles: border_color errors",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, border_color = 1),
               "")
})

test_that("sprinkles: halign",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, 
                        halign = "left")$body$halign[1],
               "left")
})

test_that("sprinkles: halign errors",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, halign = c("left", "right")),
               "")
})
               
test_that("sprinkles: height",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, height = 12)$body$height[1],
               "12")
})

test_that("sprinkles: height errors",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, height = c(12, "text")),
               "")
})

test_that("sprinkles: height_units",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, 
                        height_units = "px")$body$height_units[1],
               "px")
})

test_that("sprinkles: height_units errors",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, height_units = "em"),
               "")
})

test_that("sprinkles: fn",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, 
                        fn = quote(pvalString(value)))$body$fn[1],
               "pvalString(value)")
})

test_that("sprinkles: font_color",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, 
                        font_color = "orchid")$body$font_color[1],
               "orchid")
})
           
test_that("sprinkles: font_color error",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, font_color = c("orchid", "aquamarine")),
               "")
})

test_that("sprinkles: font_size",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, 
                        font_size = 12)$body$font_size[1],
               "12")
})

test_that("sprinkles: font_size errors",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, font_size = c("1", "x")),
               "")
})

test_that("sprinkles: font_size_units",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, 
                        font_size_units = "em")$body$font_size_units[1],
               "em")
})

test_that("sprinkles: font_size_unit errors",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, font_size_units = c("inches")),
               "")
})

test_that("sprinkles: italic",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, 
                        italic = TRUE)$body$italic[1],
               TRUE)
})

test_that("sprinkles: italic errors",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, italic = c(1, 2)),
               "")
})

test_that("sprinkles: longtable accepts TRUE",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, longtable = TRUE)$longtable,
               TRUE)
})

test_that("sprinkles: longtable accepts FALSE",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, longtable = FALSE)$longtable,
               FALSE)
})

test_that("sprinkles: longtable accepts a number",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, longtable = 10)$longtable,
               10)
})

test_that("sprinkles: longtable- character resolve to FALSE",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, longtable = "character")$longtable,
               FALSE)
})

test_that("sprinkles: merge",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_silent(sprinkle(x, rows = 3:4, cols = 1:2, merge = TRUE,
                       merge_rowval = 4, merge_colval = 2))
})

test_that("sprinkles: merge is not logical",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 3:4, cols = 1:2, merge = "yes please",
                       merge_rowval = 4, merge_colval = 2),
               "")
})

test_that("sprinkles: merge_rowval without merge casts error",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 3:4, cols = 1:2,
                       merge_rowval = 4),
               "")
})

test_that("sprinkles: merge_colval without merge casts error",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 3:4, cols = 1:2,
                       merge_colval = 4),
               "")
})

test_that("sprinkles: na_string",
{
  x <- dust(aov(mpg ~ factor(am) + factor(gear), data = mtcars))
  expect_equal(sprinkle(x, na_string = "")$body$na_string,
               rep("", 18))
})

test_that("sprinkles: na_string not character",
{
  x <- dust(aov(mpg ~ factor(am) + factor(gear), data = mtcars))
  expect_error(sprinkle(x, na_string = 1),
               "")
}) 

test_that("sprinkles: pad",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, pad = 5)$body$pad[1],
               "5")
})

test_that("sprinkles: pad errors",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, pad = c(1, "text")),
               "")
})

test_that("sprinkles: replace",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, replace = "Intercept")$body$value[1],
              "Intercept")
})

test_that("sprinkles: replace errors",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, 
                        replace = c("Intercept", "1/4 Mile Time")),
               "")
})

test_that("sprinkles: round",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, round = 5)$body$round[1],
               "5")
})

test_that("sprinkles: round errors",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, round = c(1, "text")),
               "")
})

test_that("sprinkles: rotate_degree",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, 
                        rotate_degree = 5)$body$rotate_degree[1],
               "5")
})

test_that("sprinkles: rotate_degree errors",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, rotate_degree = c(1, "text")),
               "")
})

test_that("sprinkles: valign",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, valign = "middle")$body$valign[1],
               "middle")
          })

test_that("sprinkles: valign errors",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, valign = "above"),
               "")
})

test_that("sprinkles: width",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, width = 12)$body$width[1],
               "12")
})

test_that("sprinkles: width errors",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, width = c(12, "text")),
               "")
})

test_that("sprinkles: width_units",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_equal(sprinkle(x, rows = 1, cols = 1, 
                        width_units = "px")$body$width_units[1],
               "px")
})

test_that("sprinkles: width_units errors",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, width_units = "em"),
               "")
})

test_that("sprinkles: no sprinkles given",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data=mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1),
               "")
})

test_that("sprinkles: single unnamed sprinkle",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, "green"),
               "")
})

test_that("sprinkles: multiple unnamed sprinkles",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, "green", "dotted"),
               "")
})

test_that("sprinkles: mixture of named and unnamed sprinkles",
{
  x <- dust(lm(mpg ~ qsec + factor(am) + wt, data = mtcars))
  expect_error(sprinkle(x, rows = 1, cols = 1, border_color = "green", "dotted"),
               "")
})
  
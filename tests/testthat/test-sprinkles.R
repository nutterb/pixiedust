context("sprinkles")

test_that("sprinkles: bg",
{
  expect_equal(sprinkle(rows = 1, cols = 1, bg = "orchid"),
               structure(list(rows = 1, cols = 1, 
                              sprinkles = structure(list(bg = "orchid"), 
                                                    .Names = "bg"), 
                              part = "body"), 
                         .Names = c("rows", "cols", "sprinkles", "part"), class = "sprinkle"))
})

test_that("sprinkles: bg casts an error when 'bg' has length > 1",
{
  expect_error(sprinkle(rows = 1, cols = 1, bg = c("orchid", "white")),
               "Arguments in '...' must have length 1. Please check bg.")
})

test_that("sprinkles: bg_pattern",
{
  expect_equal(sprinkle(rows = 1, cols = 1, bg_pattern = c("orchid", "aquamarine")),
               structure(list(rows = 1, cols = 1, 
                              sprinkles = structure(list(bg_pattern = c("orchid", "aquamarine"), 
                                                         bg_pattern_by = "rows"), 
                                                    .Names = c("bg_pattern", "bg_pattern_by")), 
                              part = "body"), 
                         .Names = c("rows", "cols", "sprinkles", "part"), 
                         class = "sprinkle"))
})

test_that("sprinkles: bg_pattern_by",
{
  expect_equal(sprinkle(rows=1, cols=1, bg_pattern_by = "column"),
               structure(list(rows = 1, cols = 1, 
                              sprinkles = structure(list(bg_pattern_by = "columns", 
                                                         bg_pattern = c("white", "gainsboro")),
                                                    .Names = c("bg_pattern_by", "bg_pattern")), 
                              part = "body"), 
                         .Names = c("rows", "cols", "sprinkles", "part"), 
                         class = "sprinkle"))
})

test_that("sprinkles: bg_pattern_by gives error when invalid option is given",
{
  expect_error(sprinkle(rows = 1, cols = 1, bg_pattern_by = "diagonal"),
               "sprinkles[$]bg_pattern_by' should be one of")
})

test_that("sprinkles: bold",
{
  expect_equal(sprinkle(rows = 1, cols = 1, bold = TRUE),
               structure(list(rows = 1, cols = 1, 
                              sprinkles = structure(list(bold = TRUE), 
                                                    .Names = "bold"), 
                              part = "body"), 
                         .Names = c("rows", "cols", "sprinkles", "part"), 
                         class = "sprinkle"))
})

test_that("sprinkles: bold errors",
{
  expect_error(sprinkle(rows = 1, cols = 1, bold = c(1, 2)),
               paste0("1: Arguments in '...' must have length 1. Please check bold.\n",
                      "2: The 'bold' argument must be logical"))
})

test_that("sprinkles: border_collapse",
{
  expect_equal(sprinkle(rows=1, cols = 1, border_collapse = FALSE),
               structure(list(rows = 1, cols = 1, 
                              sprinkles = structure(list(border_collapse = FALSE), 
                                                    .Names = "border_collapse"), 
                              part = "body"), 
                         .Names = c("rows", "cols", "sprinkles", "part"), 
                         class = "sprinkle"))
})

test_that("sprinkles: border_collapse errors",
{
  expect_error(sprinkle(rows = 1, cols = 1, border_collapse = c(1, 0)),
               paste0("1: Arguments in '...' must have length 1. Please check border_collapse.\n",
                      "2: The 'border_collapse' argument must be logical."))
})

test_that("sprinkles: border",
{
  expect_equal(sprinkle(rows = 1, cols = 1, border = c("left", "right")),
               structure(list(rows = 1, cols = 1, 
                              sprinkles = structure(list(border = c("left", "right"), 
                                                         border_thickness = 1, 
                                                         border_units = "px", 
                                                         border_style = "solid", 
                                                         border_color = "black"), 
                                                    .Names = c("border",  "border_thickness", 
                                                               "border_units", "border_style", 
                                                               "border_color")), 
                              part = "body"), 
                         .Names = c("rows", "cols", "sprinkles", "part"), 
                         class = "sprinkle"))
})

test_that("sprinkles: border errors",
{
  expect_error(sprinkle(rows = 1, cols = 1, border = "everything"),
               "1: 'sprinkles[$]border' should be one of")
})

test_that("sprinkles: border_thickness",
{
  expect_equal(sprinkle(rows = 1, cols = 1, border_thickness = 2),
               structure(list(rows = 1, cols = 1, 
                              sprinkles = 
                                structure(list(border_thickness = 2, 
                                               border = "all", border_units = "px",
                                               border_style = "solid", border_color = "black"), 
                                          .Names = c("border_thickness", "border", 
                                                     "border_units", "border_style", 
                                                     "border_color")), 
                              part = "body"), 
                         .Names = c("rows", "cols", "sprinkles", "part"), 
                         class = "sprinkle"))
})

test_that("sprinkles: border_thickness errors",
{
  expect_error(sprinkle(rows = 1, cols = 1, border_thickness = c(1, 2)),
               "1: Arguments in '...' must have length 1. Please check border_thickness.")
})

test_that("sprinkles: border_units",
{
  expect_equal(sprinkle(rows = 1, cols = 1, border_units = "px"),
               structure(list(rows = 1, cols = 1, 
                              sprinkles = 
                                structure(list(border_units = "px", border = "all", 
                                               border_thickness = 1, 
                                               border_style = "solid", border_color = "black"), 
                                          .Names = c("border_units", "border", "border_thickness", 
                                                     "border_style", "border_color")), 
                              part = "body"), 
                         .Names = c("rows", "cols", "sprinkles", "part"), 
                         class = "sprinkle"))
})

test_that("sprinkles: border_units errors",
{
  expect_error(sprinkle(rows = 1, cols = 1, border_units = c("inches")),
               "1: 'sprinkles[$]border_units' should be one of")
})

test_that("sprinkles: border_style",
{
  expect_equal(sprinkle(rows = 1, cols = 1, border_style = "dashed"),
               structure(list(rows = 1, cols = 1, 
                              sprinkles = 
                                structure(list(border_style = "dashed", 
                                               border = "all", border_thickness = 1,
                                               border_units = "px",  
                                               border_color = "black"), 
                                          .Names = c("border_style", "border", "border_thickness", 
                                                     "border_units", "border_color")), 
                              part = "body"), 
                         .Names = c("rows", "cols", "sprinkles", "part"), 
                         class = "sprinkle"))
})

test_that("sprinkles: border_style errors",
{
  expect_error(sprinkle(rows = 1, cols = 1, border_style = c("halo")),
               "1: 'sprinkles[$]border_style' should be one of")
})

test_that("sprinkles: border_color",
{
  expect_equal(sprinkle(rows = 1, cols = 1, border_color = "black"),
               structure(list(rows = 1, cols = 1, 
                              sprinkles = 
                                structure(list(border_color = "black", 
                                               border = "all", border_thickness = 1,
                                               border_units = "px",  
                                               border_style = "solid"), 
                                          .Names = c("border_color", "border", "border_thickness", 
                                                     "border_units", "border_style")), 
                              part = "body"), 
                         .Names = c("rows", "cols", "sprinkles", "part"), 
                         class = "sprinkle"))
})

test_that("sprinkles: border_color errors",
{
  expect_error(sprinkle(rows = 1, cols = 1, border_color = 1),
               "1: The 'border_color' argument must be a character string.")
})

test_that("sprinkles: halign",
{
  expect_equal(sprinkle(rows = 1, cols = 1, halign = "left"),
               structure(list(rows = 1, cols = 1, 
                              sprinkles = structure(list(halign = "left"), 
                                                    .Names = "halign"), 
                              part = "body"), 
                         .Names = c("rows", "cols", "sprinkles", "part"), 
                         class = "sprinkle"))
})

test_that("sprinkles: halign errors",
{
  expect_error(sprinkle(rows = 1, cols = 1, halign = c("left", "right")),
               paste0("1: Arguments in '...' must have length 1. Please check halign.\n",
                      "2: 'sprinkles[$]halign' must be of length 1\n",
                      "3: there is more than one match in 'match_arg'"))
          })
               
test_that("sprinkles: height",
{
  expect_equal(sprinkle(rows = 1, cols = 1, height = 12),
               structure(list(rows = 1, cols = 1, 
                              sprinkles = structure(list(height = 12, height_units = "px"), 
                                                    .Names = c("height", "height_units")), 
                              part = "body"), 
                         .Names = c("rows", "cols", "sprinkles", "part"), 
                         class = "sprinkle"))
})

test_that("sprinkles: height errors",
{
  expect_error(sprinkle(rows = 1, cols = 1, height = c(12, "text")),
               paste0("1: Arguments in '...' must have length 1. Please check height.\n",
                      "2: The 'height' argument must be numeric"))
})

test_that("sprinkles: height",
{
  expect_equal(sprinkle(rows = 1, cols = 1, height_units = "px"),
               structure(list(rows = 1, cols = 1, 
                              sprinkles = structure(list(height_units = "px"), 
                                                    .Names = "height_units"), 
                              part = "body"), 
                         .Names = c("rows", "cols", "sprinkles", "part"), class = "sprinkle"))
})

test_that("sprinkles: height errors",
{
  expect_error(sprinkle(rows = 1, cols = 1, height_units = "em"),
               "1: 'sprinkles[$]height_units' should be one of")
})


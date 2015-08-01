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
#   expect_error(sprinkle(rows = 1, cols = 1, bg_pattern_by = "diagonal"),
#                "sprinkles[$]bg_pattern_by' should be one of")
  expect_that(sprinkle(rows = 1, cols = 1, bg_pattern_by = "diagonal"),
              throws_error())
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
#   expect_error(sprinkle(rows = 1, cols = 1, border = "everything"),
#                "1: 'sprinkles[$]border' should be one of")
  expect_that(sprinkle(rows = 1, cols = 1, border = "everything"),
              throws_error())
})

test_that("sprinkles: border_thickness",
{
  expect_equal(sprinkle(rows = 1, cols = 1, border_thickness = 2),
               structure(list(rows = 1, cols = 1, 
                              sprinkles = 
                                structure(list(border_thickness = 2, 
                                               border = c("left", "right", "top", "bottom"), 
                                               border_units = "px",
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
                                structure(list(border_units = "px", 
                                               border = c("left", "right", "top", "bottom"), 
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
#   expect_error(sprinkle(rows = 1, cols = 1, border_units = c("inches")),
#                "1: 'sprinkles[$]border_units' should be one of")
  expect_that(sprinkle(rows = 1, cols = 1, border_units = c("inches")),
              throws_error())
})

test_that("sprinkles: border_style",
{
  expect_equal(sprinkle(rows = 1, cols = 1, border_style = "dashed"),
               structure(list(rows = 1, cols = 1, 
                              sprinkles = 
                                structure(list(border_style = "dashed", 
                                               border = c("left", "right", "top", "bottom"), 
                                               border_thickness = 1,
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
#   expect_error(sprinkle(rows = 1, cols = 1, border_style = c("halo")),
#                "1: 'sprinkles[$]border_style' should be one of")
  expect_that(sprinkle(rows = 1, cols = 1, border_style = c("halo")),
              throws_error())
})

test_that("sprinkles: border_color",
{
  expect_equal(sprinkle(rows = 1, cols = 1, border_color = "black"),
               structure(list(rows = 1, cols = 1, 
                              sprinkles = 
                                structure(list(border_color = "black", 
                                               border = c("left", "right", "top", "bottom"), 
                                               border_thickness = 1,
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
#   expect_error(sprinkle(rows = 1, cols = 1, halign = c("left", "right")),
#                paste0("1: Arguments in '...' must have length 1. Please check halign.\n",
#                       "2: 'sprinkles[$]halign' must be of length 1\n",
#                       "3: there is more than one match in 'match_arg'"))
  
  expect_that(sprinkle(rows = 1, cols = 1, halign = c("left", "right")),
              throws_error())
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

test_that("sprinkles: height_units",
{
  expect_equal(sprinkle(rows = 1, cols = 1, height_units = "px"),
               structure(list(rows = 1, cols = 1, 
                              sprinkles = structure(list(height_units = "px"), 
                                                    .Names = "height_units"), 
                              part = "body"), 
                         .Names = c("rows", "cols", "sprinkles", "part"), class = "sprinkle"))
})

test_that("sprinkles: height_units errors",
{
#   expect_error(sprinkle(rows = 1, cols = 1, height_units = "em"),
#                "1: 'sprinkles[$]height_units' should be one of")
  expect_that(sprinkle(rows = 1, cols = 1, height_units = "em"),
              throws_error())
})

test_that("sprinkles: fn",
{
  expect_equal(sprinkle(rows = 1, cols = 1, fn = quote(pvalString(value))),
               structure(list(rows = 1, cols = 1, 
                              sprinkles = structure(list(fn = "pvalString(value)"), 
                                                    .Names = "fn"), 
                              part = "body"), 
                         .Names = c("rows", "cols", "sprinkles", "part"), 
                         class = "sprinkle"))
})

test_that("sprinkles: font_color",
{
  expect_equal(sprinkle(rows = 1, cols = 1, font_color = "orchid"),
               structure(list(rows = 1, cols = 1, 
                              sprinkles = structure(list(font_color = "orchid"), 
                                                    .Names = "font_color"), 
                              part = "body"), 
                         .Names = c("rows", "cols", "sprinkles", "part"), 
                         class = "sprinkle"))
})
           
test_that("sprinkles: font_color error",
{
  expect_error(sprinkle(rows = 1, cols = 1, font_color = c("orchid", "aquamarine")),
               "1: Arguments in '...' must have length 1. Please check font_color.")
})

test_that("sprinkles: font_size",
{
  expect_equal(sprinkle(rows = 1, cols = 1, font_size = 12),
               structure(list(rows = 1, cols = 1, 
                              sprinkles = structure(list(font_size = 12, 
                                                         font_size_units = "px"), 
                                                    .Names = c("font_size","font_size_units")), 
                              part = "body"), 
                         .Names = c("rows", "cols", "sprinkles", "part"), 
                         class = "sprinkle"))
})

test_that("sprinkles: font_size errors",
{
  expect_error(sprinkle(rows = 1, cols = 1, font_size = c("1", "x")),
               paste0("1: Arguments in '...' must have length 1. Please check font_size.\n",
                      "2: The 'font_size' argument must be numeric"))
})

test_that("sprinkles: font_size_units",
{
  expect_equal(sprinkle(rows = 1, cols = 1, font_size_units = "em"),
               structure(list(rows = 1, cols = 1, 
                              sprinkles = structure(list(font_size_units = "em"), 
                                                    .Names = "font_size_units"), 
                              part = "body"), 
                         .Names = c("rows", "cols", "sprinkles", "part"), 
                         class = "sprinkle"))
})

test_that("sprinkles: font_size_unit errors",
{
#   expect_error(sprinkle(rows = 1, cols = 1, font_size_units = c("inches")),
#                paste0("1: 'sprinkles[$]font_size_units' should be one of"))
  expect_that(sprinkle(rows = 1, cols = 1, font_size_units = c("inches")),
              throws_error())
})

test_that("sprinkles: italic",
{
  expect_equal(sprinkle(rows = 1, cols = 1, italic = TRUE),
               structure(list(rows = 1, cols = 1, 
                              sprinkles = structure(list(italic = TRUE), 
                                                  .Names = "italic"), 
                              part = "body"), 
                         .Names = c("rows", "cols", "sprinkles", "part"), 
                         class = "sprinkle"))
})

test_that("sprinkles: italic errors",
{
  expect_error(sprinkle(rows = 1, cols = 1, italic = c(1, 2)),
               paste0("1: Arguments in '...' must have length 1. Please check italic.\n",
                      "2: The 'italic' argument must be logical"))
})

test_that("sprinkles: pad",
{
  expect_equal(sprinkle(rows = 1, cols = 1, pad = 5),
               structure(list(rows = 1, cols = 1, 
                              sprinkles = structure(list(pad = 5), 
                                                    .Names = "pad"), 
                              part = "body"), 
                         .Names = c("rows", "cols", "sprinkles", "part"), 
                         class = "sprinkle"))
})

test_that("sprinkles: pad errors",
{
  expect_error(sprinkle(rows = 1, cols = 1, pad = c(1, "text")),
               paste0("1: Arguments in '...' must have length 1. Please check pad.\n",
                      "2: The 'pad' argument must be numeric"))
})

test_that("sprinkles: round",
{
  expect_equal(sprinkle(rows = 1, cols = 1, round = 5),
               structure(list(rows = 1, cols = 1, 
                              sprinkles = structure(list(round = 5), 
                                                    .Names = "round"), 
                              part = "body"), 
                         .Names = c("rows", "cols", "sprinkles", "part"), 
                         class = "sprinkle"))
})

test_that("sprinkles: round errors",
{
  expect_error(sprinkle(rows = 1, cols = 1, round = c(1, "text")),
               paste0("1: Arguments in '...' must have length 1. Please check round.\n",
                      "2: The 'round' argument must be numeric"))
})

test_that("sprinkles: rotate_degree",
{
  expect_equal(sprinkle(rows = 1, cols = 1, rotate_degree = 5),
               structure(list(rows = 1, cols = 1, 
                              sprinkles = structure(list(rotate_degree = 5), 
                                                    .Names = "rotate_degree"), 
                              part = "body"), 
                         .Names = c("rows", "cols", "sprinkles", "part"), 
                         class = "sprinkle"))
})

test_that("sprinkles: rotate_degree errors",
{
  expect_error(sprinkle(rows = 1, cols = 1, rotate_degree = c(1, "text")),
               paste0("1: Arguments in '...' must have length 1. Please check rotate_degree.\n",
                      "2: The 'rotate_degree' argument must be numeric"))
})

test_that("sprinkles: valign",
{
  expect_equal(sprinkle(rows = 1, cols = 1, valign = "middle"),
               structure(list(rows = 1, cols = 1, 
                              sprinkles = structure(list(valign = "middle"), 
                                                    .Names = "valign"), 
                              part = "body"), 
                         .Names = c("rows", "cols", "sprinkles", "part"), 
                         class = "sprinkle"))
          })

test_that("sprinkles: valign errors",
{
#   expect_error(sprinkle(rows = 1, cols = 1, valign = "above"),
#                paste0("1: 'sprinkles[$]valign' should be one of"))
  expect_that(sprinkle(rows = 1, cols = 1, valign = "above"),
              throws_error())
})

test_that("sprinkles: width",
{
  expect_equal(sprinkle(rows = 1, cols = 1, width = 12),
               structure(list(rows = 1, cols = 1, 
                              sprinkles = structure(list(width = 12, width_units = "px"), 
                                                    .Names = c("width", "width_units")), 
                              part = "body"), 
                         .Names = c("rows", "cols", "sprinkles", "part"), 
                         class = "sprinkle"))
})

test_that("sprinkles: width errors",
{
  expect_error(sprinkle(rows = 1, cols = 1, width = c(12, "text")),
               paste0("1: Arguments in '...' must have length 1. Please check width.\n",
                      "2: The 'width' argument must be numeric"))
})

test_that("sprinkles: width_units",
{
  expect_equal(sprinkle(rows = 1, cols = 1, width_units = "px"),
               structure(list(rows = 1, cols = 1, 
                              sprinkles = structure(list(width_units = "px"), 
                                                    .Names = "width_units"), 
                              part = "body"), 
                         .Names = c("rows", "cols", "sprinkles", "part"), class = "sprinkle"))
})

test_that("sprinkles: width_units errors",
{
#   expect_error(sprinkle(rows = 1, cols = 1, width_units = "em"),
#                "1: 'sprinkles[$]width_units' should be one of")
  expect_that(sprinkle(rows = 1, cols = 1, width_units = "em"),
              throws_error())
})
#' @name cell_attribute_bunnies
#' @importFrom lazyWeave pvalString
#' @export pvalString
#' @aliases dust_fn, dust_bold, dust_italic
#' 
#' @title Apply Functions to cells in a table
#' @description Some cells may require particular formatting or additional calculation prior to
#'   rendering.  Examples of such cells are p-values and currencies.  Applying a function to 
#'   these cells allows them to be formatted at the rendering time rather than having to 
#'   format them into strings prior to rendering.
#'   
#' @param ... Named argument with characteristics described below.
#' 
#' @details These are intended to be simple functions, mostly for formatting.  Calculations and
#'   advanced manipulations should be avoided.  Ideally, these should format numeric values or 
#'   potentially perform regular expression manipulations.
#' 
#' @section Input Formats:
#' \itemize{
#'   \item{\code{row}}{ Integers giving rows across which a function is applied.  The function is always 
#'     applied on the intersection of `row` and `col` (or `colname`).}
#'   \item{\code{col}}{ Integers giving columns over which a function is applied.  The function is always 
#'     applied on the intersection of `row` and `col`.}
#'   \item{\code{colname}}{ A character vector of column names over which the column is applied.  The function is always 
#'     applied on the intersection of `row` and `colname`.}
#' }
#' 
#' @author Benjamin Nutter
#' 
#' @examples 
#' x <- dust(lm(mpg ~ qsec + factor(am), data = mtcars))
#' x
#' 
#' x + dust_fn(colname = "p.value", fn = quote(pvalString(value)))
#' 
#' x + dust_fn(colname = "statistic", fn = quote(format(value, digits = 3))) 
#' 
#' x + 
#' dust_fn(colname = "statistic", fn = quote(format(value, digits = 3))) + 
#' dust_fn(colname = "p.value", fn = quote(pvalString(value)))
#' 
#' x + dust_fn(colname = "estimate", fn = quote(format(value, digits = 3))) + 
#'   dust_bold(colname = "estimate", row = 2, set_bold = TRUE)
#'   
#' x + dust_fn(colname = "estimate", fn = quote(format(value, digits = 3))) + 
#'   dust_italic(colname = "estimate", row = 2, set_italic = TRUE)
#'   
#' x + dust_round(colname = c("estimate", "statistic", "std.error"), round = 3) + 
#'   dust_fn(colname = "p.value", fn = quote(pvalString(value))) + 
#'   dust_colnames("Term", "Estimate", "SE", "T statistic", "p-value")

NULL

#' @rdname cell_attribute_bunnies
#' @param across A character string of length one.  Must be either \code{row} or \code{column} and
#'   accepts partial matching.
#' @param colors A character vector of colors to recycle in the pattern.
#' @export

dust_bg_pattern <- function(across = c("row", "column"), 
                            colors = c("#F0F0F0", "#BDBDBD")){
  Check <- ArgumentCheck::newArgCheck()
  
  across <- ArgumentCheck::match_arg(arg = across, 
                                     choices = c("row", "column"),
                                     argcheck = Check)
  if (length(across) == 0)
    ArgumentCheck::finishArgCheck(Check)
  
  structure(list(across = across,
                 colors = colors),
            class = c("dust_bg_pattern", "dust_bunny"))
}

#' @rdname cell_attribute_bunnies
#' @param set_bold Logical. Sets the \code{bold} flag for the table.
#' @export

dust_bold <- function(..., set_bold)
{
  Check <- ArgumentCheck::newArgCheck()
  dust_list <- dust_list_checks(..., attr = set_bold, fn = "dust_bold", argcheck = Check)

  if (any(!is.logical(dust_list$set_bold)))
    ArgumentCheck::addError(
      msg = "'set_bold' must be logical.",
      argcheck = Check)
  
  ArgumentCheck::finishArgCheck(Check)
  
  structure(dust_list,
            class = c("dust_bold", "dust_bunny"))
}

#' @rdname cell_attribute_bunnies
#' @param color A character string naming the background color for the cells
#' @export

dust_cell_bg <- function(..., color)
{
  Check <- ArgumentCheck::newArgCheck()
  dust_list <- dust_list_checks(..., attr = color, fn = "dust_cell_bg", argcheck = Check)
  
  if (any(!is.character(dust_list$color)))
    ArgumentCheck::addError(
      msg = "'color' must be a character value.",
      argcheck = Check)
  
  ArgumentCheck::finishArgCheck(Check)
  
  structure(dust_list,
            class = c("dust_cell_bg", "dust_bunny"))
}

#' @rdname cell_attribute_bunnies
#' @param sides A character vector of up to length 4.  May use any of \code{"top"}, \code{"bottom"},
#'   \code{"left"} or \code{"right"}.  The border style is applied to the sides of the table
#'   specified.  Multiple sides are accepted and partial matching is performed.
#' @param thickness A numeric vector of length 1 specifying the thickness of the border.
#' @param style A character string giving the style for the border line.  Only the first value is 
#'   accepted.
#' @export

dust_cell_border <- function(..., sides, thickness = 1, units = c("px", "pt"),
                             style = c("solid", "dashed", "dotted"),
                             color = "black")
{
  Check <- ArgumentCheck::newArgCheck()
  
  sides <- ArgumentCheck::match_arg(sides, c("left", "right", "top", "bottom"),
                                    several.ok = TRUE, argcheck = Check)
  units <- ArgumentCheck::match_arg(units, c("px", "pt"), argcheck = Check)
  style <- ArgumentCheck::match_arg(style, c("solid", "dashed", "dotted"), argcheck = Check)
  
  if (length(thickness) != 1)
    ArgumentCheck::addError(
      msg = "'thickness' must have length 1.",
      argcheck = Check)
  
  if (!is.character(color) | length(color) != 1)
    ArgumentCheck::addError(
      msg = "'color' must be a character string of length 1.",
      argcheck = Check)
  
  dust_list <- dust_list_checks(..., attr = style, fn = "dust_cell_border", argcheck = Check)
  
  ArgumentCheck::finishArgCheck(Check)
  
  style <- paste0(thickness, units, " ", style, " ", color)
  
  dust_list$style <- style
  dust_list$sides <- sides
  
  structure(dust_list,
            class = c("dust_cell_border", "dust_bunny"))
}

#' @rdname cell_attribute_bunnies
#' @param height A character string or numeric value giving the height of the cell
#' @param units Units for the measurement.  Options may vary depending on the 
#'   attribute being modified, but the full list of valid options is included
#'   in the argument's default vector.
#' @export

dust_cell_height <- function(..., height, units = c("px"))
{
  Check <- ArgumentCheck::newArgCheck()
  units <- ArgumentCheck::match_arg(units, 
                                    c("px"), 
                                    argcheck = Check)

  dust_list <- dust_list_checks(..., attr = height, fn = "dust_cell_height", argcheck = Check)
  
  if (any(!is.character(dust_list$height) & !is.numeric(dust_list$height)))
    ArgumentCheck::addError(
      msg = "'height' must be either a character or numeric value.",
      argcheck = Check)
  
  ArgumentCheck::finishArgCheck(Check)
  
  dust_list$height <- paste0(dust_list$height, units)
  
  structure(dust_list,
            class = c("dust_cell_height", "dust_bunny"))
}

#' @rdname cell_attribute_bunnies
#' @param width A character string or numeric value giving the width of the cell.
#' @export

dust_cell_width <- function(..., width, units = c("%", "px"))
{
  Check <- ArgumentCheck::newArgCheck()
  units <- ArgumentCheck::match_arg(units, 
                                    c("%", "px"), 
                                    argcheck = Check)
  
  dust_list <- dust_list_checks(..., attr = width, fn = "dust_cell_width", argcheck = Check)
  
  if (any(!is.character(dust_list$width) & !is.numeric(dust_list$width)))
    ArgumentCheck::addError(
      msg = "'width' must be either a character or numeric value.",
      argcheck = Check)
  
  ArgumentCheck::finishArgCheck(Check)
  
  dust_list$width <- paste0(dust_list$width, units)
  
  structure(dust_list,
            class = c("dust_cell_width", "dust_bunny"))
}

#' @rdname cell_attribute_bunnies
#' @param halign A character value.  May be any of \code{"l"}, \code{"left"}, \code{"c"}, 
#'   \code{"center"}, \code{"r"}, or \code{"right"}.  Truthfully, only the first letter matters,
#'   so submitting \code{halign = "cowabunga dude"} will center align a cell.  I've chosen to 
#'   allow this oddity since it will be forgiving of unintentional misspellings.
#' @export

dust_cell_halign <- function(..., halign){
  Check <- ArgumentCheck::newArgCheck()
  dust_list <- dust_list_checks(..., attr = halign, fn = "dust_cell_halign", argcheck = Check)
  
  dust_list$halign <- tolower(substr(dust_list$halign, 1, 1))
  
  if (any(!halign %in% c("l", "left", "c", "center", "r", "right")))
    ArgumentCheck::addError(
      msg = paste0("'dust_cell_halign' argument 'halign' only accepts ",
                   "'l', 'left', 'c', 'center', 'r', and 'right'."),
      argcheck = Check)
  
  ArgumentCheck::finishArgCheck(Check)
  
  structure(dust_list,
            class = c("dust_cell_halign", "dust_bunny"))
}

#' @rdname cell_attribute_bunnies
#' @param valign A character value.  May be any of \code{"m"}, \code{"middle"}, \code{"b"}, 
#'   \code{"bottom"}, \code{"t"}, or \code{"top"}.  Truthfully, only the first letter matters,
#'   so submitting \code{halign = "muggles"} will align text in the center of a cell.  I've chosen to 
#'   allow this oddity since it will be forgiving of unintentional misspellings. Abuse the 
#'   privilege at your leisure.
#' @export

dust_cell_valign <- function(..., valign){
  Check <- ArgumentCheck::newArgCheck()
  dust_list <- dust_list_checks(..., attr = valign, fn = "dust_cell_valign", argcheck = Check)
  
  dust_list$valign <- tolower(substr(dust_list$valign, 1, 1))
  
  if (any(!valign %in% c("m", "b", "t")))
    ArgumentCheck::addError(
      msg = paste0("'dust_cell_valign' argument 'valign' only accepts ",
                   "'m', 'middle', 'b', 'bottom', 't', and 'top'."),
      argcheck = Check)
  
  ArgumentCheck::finishArgCheck(Check)
  
  structure(dust_list,
            class = c("dust_cell_valign", "dust_bunny"))
}

#' @rdname cell_attribute_bunnies
#' @param fn An expression with the function to be applied to the tabulated values.  The object
#'     on which `fn` should act is `value`. (ie, `quote(format(value, big.mark=","))`)
#' @export
dust_fn <- function(..., fn)
{
  Check <- ArgumentCheck::newArgCheck()
  dust_list <- dust_list_checks(..., attr = fn, fn = "dust_fn", argcheck = Check)
  
  ArgumentCheck::finishArgCheck(Check)
  
  structure(dust_list,
            class = c("dust_fn", "dust_bunny"))
}

#' @rdname cell_attribute_bunnies
# color is documented in dust_cell_bg
#' @export

dust_font_color <- function(..., color)
{
  Check <- ArgumentCheck::newArgCheck()
  dust_list <- dust_list_checks(..., attr = color, fn = "dust_font_color", argcheck = Check)
  
  if (!is.character(dust_list$color))
    ArgumentCheck::addError(
      msg = "'color' argument in 'dust_font_color' must be a character string",
      argcheck = Check)
  
  ArgumentCheck::finishArgCheck(Check)
  
  structure(dust_list,
            class = c("dust_font_color", "dust_bunny"))
}

#' @rdname cell_attribute_bunnies
#' @param size Font size for the text.
#' @export

dust_font_size <- function(..., size, units = c("%", "px", "pt", "em"))
{
  Check <- ArgumentCheck::newArgCheck()

  units <- ArgumentCheck::match_arg(units, 
                                    c("%", "px", "pt", "em"),
                                    argcheck = Check)

  dust_list <- dust_list_checks(..., attr = size, fn = "dust_font_size", argcheck = Check)
  
  if (!is.character(dust_list$size) & !is.numeric(dust_list$size))
    ArgumentCheck::addError(
      msg = "'size' argument in 'dust_font_size' must be either numeric or a character string",
      argcheck = Check)
  
  ArgumentCheck::finishArgCheck(Check)
  
  dust_list$size = paste0(dust_list$size, units)
  
  structure(dust_list,
            class = c("dust_font_size", "dust_bunny"))
}

#' @rdname cell_attribute_bunnies
#' @param set_italic Logical. Sets the \code{italic} flag for the table.
#' @export

dust_italic <- function(..., set_italic)
{
  Check <- ArgumentCheck::newArgCheck()
  dust_list <- dust_list_checks(..., attr = set_italic, fn = "dust_italic", argcheck = Check)
  
  if (any(!is.logical(dust_list$set_italic)))
    ArgumentCheck::addError(
      msg = "'set_italic' must be logical.",
      argcheck = Check)
  
  ArgumentCheck::finishArgCheck(Check)
  
  structure(dust_list,
            class = c("dust_italic", "dust_bunny"))
}

#' @rdname cell_attribute_bunnies
#' @param round Integer.  Determines the number of decimals to which a value is rounded via
#'   the `round` function.
#' @export

dust_round <- function(..., round)
{
  Check <- ArgumentCheck::newArgCheck()
  dust_list <- dust_list_checks(..., attr = round, fn = "dust_round", argcheck = Check)
  
  if (!is.numeric(dust_list$round))
    ArgumentCheck::addError(
      msg = "'round' argument in 'dust_round' must be numeric",
      argcheck = Check)
  
  ArgumentCheck::finishArgCheck(Check)
  
  structure(dust_list,
            class = c("dust_round", "dust_bunny"))
}

#*********************************************
# dust_list checks
#*********************************************

dust_list_checks <- function(..., attr, fn, argcheck)
{
  if (length(attr) != 1 && !is.call(attr))
    ArgumentCheck::addError(
      msg = paste0("'", substitute(attr), "' must have length 1"),
      argcheck = argcheck)
  
  dust_list <- list(...)
  attr_list <- list(attr)
  names(attr_list) <- as.character(substitute(attr))

  if (any(names(dust_list) %in% ""))
    ArgumentCheck::addError(
      msg = paste0("All arguments to '", fn, "' must be named"),
      argcheck = argcheck)
  
  valid_args <- c("row", "col", "colname")
  if (any(!names(dust_list) %in% valid_args)){
    bad_args <- names(dust_list)[!names(dust_list) %in% valid_args]
    ArgumentCheck::addError(
      msg = paste0("'", paste0(bad_args, collapse="', '"), "' are not valid arguments to ",
                   fn),
      argcheck = argcheck)
  }

  dust_list <- c(dust_list, attr_list)
    
  return(dust_list)
}
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
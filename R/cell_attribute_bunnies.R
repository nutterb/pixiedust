#' @name cell_attribute_bunnies
#' @importFrom lazyWeave pvalString
#' @export pvalString
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
#'   \item{row}{Integers giving rows across which a function is applied.  The function is always 
#'     applied on the intersection of `row` and `col` (or `colname`).}
#'   \item{col}{Integers giving columns over which a function is applied.  The function is always 
#'     applied on the intersection of `row` and `col`.}
#'   \item{colname}{A character vector of column names over which the column is applied.  The function is always 
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

NULL

#' @rdname cell_attribute_bunnies
#' @param An expression with the function to be applied to the tabulated values.  The object
#'     on which `fn` should act is `value`. (ie, `quote(format(value, big.mark=","))`)
#' @export
dust_fn <- function(..., fn)
{
  Check <- ArgumentCheck::newArgCheck()
  
  dust_fn_list <- c(list(...), list(fn = fn))
  
  if (any(names(dust_fn_list) %in% ""))
    ArgumentCheck::addError(
      msg = "All arguments to 'dust_fn' must be named",
      argcheck = Check)
  
  structure(dust_fn_list,
            class = c("dust_fn", "dust_bunny"))
}

#' @rdname cell_attribute_bunnies
#' @param set_bold Logical. Sets the \code{bold} flag for the table.
#' @export

dust_bold <- function(..., set_bold)
{
  Check <- ArgumentCheck::newArgCheck()
  
  if (length(set_bold) != 1)
    ArgumentCheck::addError(
      msg = "'set_bold' must have length 1",
      argcheck = Check)
  
  dust_bold_list <- c(list(...), list(set_bold = set_bold))
  
  if (any(names(dust_bold_list) %in% ""))
    ArgumentCheck::addError(
      msg = "All arguments to 'dust_bold' must be named",
      argcheck = Check)
  
  ArgumentCheck::finishArgCheck(Check)
  
  structure(dust_bold_list,
            class = c("dust_bold", "dust_bunny"))
}
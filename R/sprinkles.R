#' @name sprinkles
#' @importFrom ArgumentCheck newArgCheck
#' @importFrom ArgumentCheck addError
#' @importFrom ArgumentCheck finishArgCheck
#' 
#' @title Define Customization to a Table
#' @description Customizations to a \code{dust} table are added by "sprinkling"
#'   with a little extra fairy dust.  Sprinkles are a collection of attributes
#'   to be applied over a subset of table cells.  They may be added to any 
#'   part of the table, or to the table as a whole.
#'   
#' @param rows A numeric vector specifying the rows of the table to sprinkle.
#'   See details for more about sprinkling.
#' @param cols A numeric (or character) vector specifying the columns (or 
#'   column names) to sprinkle.  See details for more about sprinkling.
#' @param part A character string denoting which part of the table to modify.
#' @param ... named arguments, each of length 1, defining the customizations
#'   for the given cells.  See "Sprinkles" for a listing of these arguments.
#'   
#' @details Sprinkling is done over the intersection of rows and columns.  If
#'   rows but no columns are specified, sprinkling is performed over all columns
#'   of the given given rows. The reverse is true for when columns but no rows
#'   are specified.  If neither columns nor rows are specified, the attribute 
#'   is applied over all of the cells in the table part denoted in \code{part}.
#'
#'   Whenever \code{part = "table"}, \code{rows} and \code{columns} are ignored
#'   and the attributes are applied to the entire table.
#'   
#' @section Sprinkles:
#' The following list describes the valid sprinkles that may be defined in the 
#' \code{...} dots argument.
#' 
#' \itemize{
#'   \item{\code{bg} }{}
#'   \item{\code{bg_pattern }{}
#'   \item{\code{bold }{}
#'   \item{\code{border_collapse }{}
#'   \item{\code{fn }{}
#'   \item{\code{font_color }{}
#'   \item{\code{font_size }{}
#'   \item{\code{halign }{}
#'   \item{\code{height }{}
#'   \item{\code{italic }{}
#'   \item{\code{pad }{}
#'   \item{\code{rotate_text }{}
#'   \item{\code{round }{}
#'   \item{\code{valign }{}
#'   \item{\code{width }{}
#' }
#' 
#' @author Benjamin Nutter
#'    
c("bg", "bg_pattern", "bold", "border_collapse",
  "fn", "font_color", "font_size", "halign", 
  "height", "italic", "pad", "rotate_text", 
  "round", "valign", "width")

sprinkle <- function(rows, cols, ..., 
                     part = c("body", "head", "foot", "interfoot", "table"))
{
  Check <- ArgumentCheck::newArgCheck()
  part <- ArgumentCheck::match_arg(part, 
                                   c("body", "head", "foot", "interfoot", "table"),
                                   argcheck = Check)
  
  sprinkles <- list(...)
  
  if (length(sprinkles) == 0)
    ArgumentCheck::addError(
      msg = "No sprinkles declared. You must define at least one sprinkle in '...'",
      argcheck = Check)
  
  if (any(names(sprinkles) %in% ""))
    ArgumentCheck::addError(
      msg = paste0("All arguments in '...' must be named"),
      argcheck = Check)
  
  too_long <- names(sprinkles)[vapply(sprinkles, 
                                      function(x) length(x) != 1,
                                      TRUE)]
  if (length(too_long) > 0)
    ArgumentCheck::addError(
      msg = paste0("Arguments in '...' must have length 1.",
                   " Please check", paste0(too_long, collapse=", "), "."),
      argcheck = Check)
  
  bad_sprinkles <- names(sprinkles)[!names(sprinkles) %in% sprinkle_names()]
  if (length(bad_sprinkles) > 0)
    ArgumentCheck::addError(
      msg = paste0("The following arguments in '...' are not valid ",
                   "sprinkles: ", paste0(bad_sprinkles, collapse = ", ")),
      argcheck = Check)
  
  ArgumentCheck::finishArgCheck(Check)
  
  structure(list(rows = rows,
                 cols = cols,
                 sprinkles = sprinkles,
                 part = part),
            class = "sprinkle")
}

sprinkle_names <- function()
{
  c("bg", "bg_pattern", "bold", "border_collapse",
    "fn", "font_color", "font_size", "halign", 
    "height", "italic", "pad", "rotate_text", 
    "round", "valign", "width")
}
#' @name table_attribute_bunnies
#' @importFrom ArgumentCheck addError
#' @importFrom ArgumentCheck finishArgCheck
#' @importFrom ArgumentCheck newArgCheck
#' 
#' @title Column Attributes for \code{dustpan} Tables
#' @description Assigns new column attributes to a table
#' 
#' @param ... Column attributes for the table.  See 'Input Formats'
#' 
#' @section Input Formats:
#' \itemize{
#'   \item{\code{attr_vec}}{ Using \code{dust_colnames(attr_vec = c(...))}, a vector with named or
#'     unnamed elements may be provided.  If the elements are unnamed, it must have the same 
#'     length as the number of columns in the table.}
#'   \item{named arguments}{ Using \code{dust_colnames(term = "Term", estimate = "Estimate")}, 
#'     column names may be passed for all or a subset of the columns.  The existing column
#'     name will be matched against the argument name.}
#'   \item{unnamed arguments}{ Using \code{dust_colnames("Term", "Estimate", "SE", ...)}, 
#'     column names may be passed for all of the columns.  If the arguments are unnamed, the 
#'     number of arguments passed must match the number of columns in the table.}
#'  }
#'  When using named arguments (or a named vector), you may not mix named and unnamed elements.  
#'  In other words, if one element is named, they must all be named.  Unnamed elements are assigned
#'  to columns in sequential order.
#'  
#' @author Benjamin Nutter
#' 
#' @examples
#' x <- dust(lm(mpg ~ qsec + factor(am), data = mtcars)) 
#' x
#' x + dust_colnames(term = "Term", statistic = "T")
#' x + dust_colnames("Term", "Estimate", "SE", "T-statistic", "p-value")
#' \dontrun{
#' # Causes an error due to too few unnamed arguments
#' x + dust_colnames("Term", "Estimate")
#' }

NULL

#' @rdname table_attribute_bunnies
#' @export 

dust_colnames <- function(...)
{
  Check <- ArgumentCheck::newArgCheck()
  new_names <- list(...)
  
  new_names <- dust_table_attr_checks(new_names, Check, "dust_colnames")
  
  structure(new_names, 
            class = c("col_names", "dust_bunny"))
}

#' @rdname table_attribute_bunnies
#' @export 

dust_head_halign <- function(...){
  Check <- ArgumentCheck::newArgCheck()
  new_halign <- list(...)
  
  new_halign <- dust_table_attr_checks(new_halign, Check, "dust_head_halign")
  new_halign <- tolower(substr(new_halign, 1, 1))
  
  if (any(!new_halign %in% c("l", "c", "r")))
    ArgumentCheck::addError(
      msg = "Alignment tags must be one of 'left', 'l', 'center', 'c', 'right', or 'r'",
      argcheck = Check)
  
  ArgumentCheck::finishArgCheck(Check)
  
  structure(new_halign, 
            class = c("dust_head_halign", "dust_bunny"))
}

#' @rdname table_attribute_bunnies
#' @param collapse Character string indicating if cell borders should blend together or 
#'   be distinct.
#' @export

dust_border_collapse <- function(collapse = c("collapse", "separate"))
{
  Check <- ArgumentCheck::newArgCheck()
  collapse <- ArgumentCheck::match_arg(collapse, c("collapse", "separate"), argcheck = Check)
  ArgumentCheck::finishArgCheck(Check)
  
  structure(collapse,
            class = c("dust_border_collapse", "dust_bunny"))
}

#' @rdname table_attribute_bunnies
#' @param sides A character vector of up to length 4.  May use any of \code{"top"}, \code{"bottom"},
#'   \code{"left"} or \code{"right"}.  The border style is applied to the sides of the table
#'   specified.  Multiple sides are accepted and partial matching is performed.
#' @param thickness A numeric vector of length 1 specifying the thickness of the border.
#' @param units A character string indicating the units of \code{thickness}.  
#' @param style A character string giving the style for the border line.  Only the first value is 
#'   accepted.
#' @param color A character string denoting the color of the border.  Only one value is permitted.
#' @export

dust_table_border <- function(sides, thickness=1, units=c("px", "pt"),
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
  
  ArgumentCheck::finishArgCheck(Check)
  
  style = paste0(thickness, units, " ", style, " ", color)
  
  
  structure(list(sides = sides,
                 style = style),
            class = c("dust_table_border", "dust_bunny"))
}


#********************************************

dust_table_attr_checks <- function(attr_list, argcheck, fn)
{
  #* Return the 'attr_vec' vector, if used.
  if ("attr_vec" %in% names(attr_list)){
    attr_list <- attr_list[["attr_vec"]]
    if (any(names(attr_list) %in% ""))
      ArgumentCheck::addError(
        msg = "Elements of 'attr_vec' must either all be named or all be unnamed",
        argcheck = argcheck)
    ArgumentCheck::finishArgCheck(argcheck)      
  }
  #* Return an error if any element in ... has length greater than 1.
  else if (any(vapply(attr_list, length, 1) != 1)){
    ArgumentCheck::addError(
      msg = paste0("Arguments to '", fn, "' should have length 1 (unless using 'attr_vec')"),
      argcheck = argcheck)
    
    ArgumentCheck::finishArgCheck(argcheck)
  }
  
  #* Return the vector when no names are used
  else if (is.null(names(attr_list))) 
    attr_list <- vapply(X = attr_list, 
                        FUN = identity, 
                        FUN.VALUE = "character", 
                        USE.NAMES = FALSE)
  
  #* Return the vector when names are used
  else if (!is.null(names(attr_list))){
    attr_list <- vapply(X = attr_list, 
                        FUN = identity,
                        FUN.VALUE = "character",
                        USE.NAMES = TRUE)
    #* Print an error in the case of named and unnamed elements
    if (any(names(attr_list) %in% "")){
      ArgumentCheck::addError(
        msg = "arguments to ... must either all be named or all be unnamed",
        argcheck = argcheck)
      ArgumentCheck::finishArgCheck(argcheck)
    }
  }
  else {
    ArgumentCheck::addError(
      msg = "End of 'dust_colnames' reached without generating output",
      argcheck = argcheck)
    ArgumentCheck::finishArgCheck(argcheck)
  } 
  
  return(attr_list)
}





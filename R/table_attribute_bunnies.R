#' @name table_attribute_bunnies
#' @importFrom ArgumentCheck addError
#' @importFrom ArgumentCheck finishArgCheck
#' @importFrom ArgumentCheck newArgCheck
#' 
#' @title Column Names for \code{dustpan} Tables
#' @description Assigns new column names to a table
#' 
#' @param ... Column names for the table.  See 'Input Formats'
#' 
#' @section Input Formats:
#' \itemize{
#'   \item{\code{col_names}}{Using \code{dust_colnames(col_names = c(...))}, a vector with named or
#'     unnamed elements may be provided.  If the elements are unnamed, it must have the same 
#'     length as the number of columns in the table.}
#'   \item{named arguments}{Using \code{dust_colnames(term = "Term", estimate = "Estimate")}, 
#'     column names may be passed for all or a subset of the columns.  The existing column
#'     name will be matched against the argument name.}
#'   \item{unnamed arguments}{Using \code{dust_colnames("Term", "Estimate", "SE", ...)}, 
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
  
  #* Return the 'col_names' vector, if used.
  if ("col_names" %in% names(new_names)){
    new_names <- new_names[["col_names"]]
    if (any(names(new_names) %in% ""))
      ArgumentCheck::addError(
        msg = "Elements of 'col_names' must either all be named or all be unnamed",
        argcheck = Check)
    ArgumentCheck::finishArgCheck(Check)      
  }
  #* Return an error if any element in ... has length greater than 1.
  else if (any(vapply(new_names, length, 1) != 1)){
    ArgumentCheck::addError(
      msg = "Arguments to 'dust_colnames' should have length 1 (unless using 'col_names')",
      argcheck = Check)
  
    ArgumentCheck::finishArgCheck(Check)
  }
    
  #* Return the vector when no names are used
  else if (is.null(names(new_names))) 
    new_names <- vapply(X = new_names, 
                        FUN = identity, 
                        FUN.VALUE = "character", 
                        USE.NAMES = FALSE)
  
  #* Return the vector when names are used
  else if (!is.null(names(new_names))){
    new_names <- vapply(X = new_names, 
                        FUN = identity,
                        FUN.VALUE = "character",
                        USE.NAMES = TRUE)
    #* Print an error in the case of named and unnamed elements
    if (any(names(new_names) %in% "")){
      ArgumentCheck::addError(
        msg = "arguments to ... must either all be named or all be unnamed",
        argcheck = Check)
      ArgumentCheck::finishArgCheck(Check)
    }
  }
  else {
    ArgumentCheck::addError(
      msg = "End of 'dust_colnames' reached without generating output",
      argcheck = Check)
    ArgumentCheck::finishArgCheck(Check)
  }
  structure(new_names, 
            class = c("col_names", "dust_bunny"))
}

#' @rdname table_attribute_bunnies
#' @param print_method A character string denoting the printing method for the table.  This 
#'   must have length 1.
#' @export 

dust_print_method <- function(print_method){
  structure(print_method,
            class = c("dust_print_method", "dust_bunny"))
}



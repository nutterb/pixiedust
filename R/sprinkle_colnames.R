#' @name sprinkle_colnames
#' @export sprinkle_colnames
#' @importFrom ArgumentCheck addError
#' @importFrom ArgumentCheck finishArgCheck
#' @importFrom ArgumentCheck newArgCheck
#' 
#' @title Column Names for \code{dust} Tables
#' @description Assigns new column names to a table
#' 
#' @param ... Column names for the table.  See 'Input Formats'
#' 
#' @section Input Formats:
#' \itemize{
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
#' x + sprinkle_colnames(term = "Term", statistic = "T")
#' x + sprinkle_colnames("Term", "Estimate", "SE", "T-statistic", "p-value")
#' \dontrun{
#' # Causes an error due to too few unnamed arguments
#' x + sprinkle_colnames("Term", "Estimate")
#' }
sprinkle_colnames <- function(...)
{
  Check <- ArgumentCheck::newArgCheck()
  new_names <- list(...)
  
  if (any(names(new_names) %in% "")){
      ArgumentCheck::addError(
        msg = "Elements of '...' must either all be named or all be unnamed",
        argcheck = Check)
    ArgumentCheck::finishArgCheck(Check)      
  }
  #* Return an error if any element in ... has length greater than 1.
  else if (any(vapply(new_names, length, 1) != 1)){
    ArgumentCheck::addError(
      msg = "Arguments to '...' should have length 1",
      argcheck = Check)
    
    ArgumentCheck::finishArgCheck(Check)
  }
  
  #* Return the vector when no names are used
  else if (is.null(names(new_names))){
    new_names <- vapply(X = new_names, 
                        FUN = identity, 
                        FUN.VALUE = "character", 
                        USE.NAMES = FALSE)
  }
  
  #* Return the vector when names are used
  else if (!is.null(names(new_names))){
    new_names <- vapply(X = new_names, 
                        FUN = identity,
                        FUN.VALUE = "character",
                        USE.NAMES = TRUE)
  }

  structure(new_names, 
            class = c("col_names", "sprinkle"))
}
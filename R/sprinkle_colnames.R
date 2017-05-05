#' @name sprinkle_colnames
#' @export sprinkle_colnames
#' 
#' @title Column Names for \code{dust} Tables
#' @description Assigns new column names to a table
#' 
#' @param x A dust object.
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
#' @seealso \code{\link{sprinkle}}
#' 
#' @examples
#' x <- dust(lm(mpg ~ qsec + factor(am), data = mtcars)) 
#' x
#' x %>% sprinkle_colnames(term = "Term", statistic = "T")
#' x %>% sprinkle_colnames("Term", "Estimate", "SE", "T-statistic", "p-value")
#' \dontrun{
#' # Causes an error due to too few unnamed arguments
#' x %>% sprinkle_colnames("Term", "Estimate")
#' }

#' @rdname sprinkle_colnames
#' @export

sprinkle_colnames <- function(x, ...)
{
  UseMethod("sprinkle_colnames")
}

#' @rdname sprinkle_colnames
#' @export

sprinkle_colnames.default <- function(x, ...)
{
  coll <- checkmate::makeAssertCollection()

  checkmate::assertClass(x = x,
                         classes = "dust")

  new_names <- list(...)
  
  if (any(names(new_names) %in% ""))
  {
    coll$push("Elements of '...' must either all be named or all be unnamed")
  }
  #* Return an error if any element in ... has length greater than 1.
  else if (any(vapply(new_names, length, 1) != 1)){
    coll$push("Arguments to '...' should have length 1")
    checkmate::reportAssertions(coll)
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

  if (!is.null(names(new_names))){
    if (any(!names(new_names) %in% x$head$col_name))
    {
      bad_names <- names(new_names)[!names(new_names) %in% x$head$col_name]
      coll$push(sprintf("The following variable names are not found in the dust table:\n    %s",
                        paste0(bad_names, collapse=", ")))
    }
    checkmate::reportAssertions(coll)
    
    x$head$value[match(names(new_names), x$head$col_name)] <- new_names
  } 
  else{
    checkmate::assertCharacter(x = new_names,
                               len = max(x[["head"]][["col"]]),
                               add = coll)
    x$head$value <- new_names
  }
  x
}

#' @rdname sprinkle_colnames
#' @export

sprinkle_colnames.dust_list <- function(x, ...)
{
  structure(
    lapply(X = x,
           FUN = sprinkle_colnames,
           ...),
    class = "dust_list"
  )
}

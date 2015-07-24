#' @name add_bunnies
#' @export
#' @importFrom ArgumentCheck addError
#' @importFrom ArgumentCheck finishArgCheck
#' @importFrom ArgumentCheck newArgCheck
#' @method + dust
#' 
#' @title Modify Attributes of a \code{dustpan} table
#' @description Adds attributes of a plot to be rendered by the \code{print.dust} method.
#' 
#' @param x a \code{dust} object
#' @param y a \code{dust_bunny} object.
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


'+.dust' <- function(x, y)
{
  #* Not quite sure how to make this one work yet
  #   Check <- ArgumentCheck::newArgCheck()
  #   if (!"dust_bunny" %in% class(y))
  #     ArgumentCheck::addError(
  #       msg = paste0(substitute(y), " is not a dust_bunny object"),
  #       argcheck = Check)
  
  # ArgumentCheck::finishArgCheck(Check)
  dust_bunny_type <- class(y)[1]
  switch(dust_bunny_type,
         "col_names" = add_colnames(x, y),
         stop(paste0("dust_bunny_type '", dust_bunny_type, "' not recognized.")))
}

add_colnames <- function(x, y)
{
  Check <- ArgumentCheck::newArgCheck()
  
  old_names <- x$col_names
  
  if (is.null(names(y))){
    if (length(y) != length(old_names)){
      ArgumentCheck::addError(
        msg = paste0("Unnamed column names from 'dust_colnames' should have length ", 
                     length(old_names), "."),
        argcheck = Check)
      ArgumentCheck::finishArgCheck(Check)
    }
    x$col_names <- unclass(y)
  } 
  else{
    x$col_names[match(names(y), old_names)] <- y
  }
  
  return(x)
}
    



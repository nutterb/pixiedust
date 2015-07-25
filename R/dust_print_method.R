#' @name dust_print_method
#' @export
#' 
#' @title Set the Print Method for a \code{dust} Table
#' @details \code{dust} supports printing to the console, markdown, html, and latex.  By default,
#'   tables are printed to the console, unless the user changes \code{options()$dust_format}.
#'   Printing methods may be changed in any dust call by invoking \code{dust_print_format}.
#'   
#' @param print_method A character string denoting the printing method for the table.  This 
#'   must have length 1.
#' 
#' @author Benjamin Nutter
#' 
#' @examples 
#' x <- dust(lm(mpg ~ qsec + factor(am), data = mtcars))
#' 
#' x$print_method
#' 
#' x <- x + dust_print_method("html")
#' x$print_method
#' 
#' x <- x + dust_print_method("markdown")
#' x$print_method
#' 

dust_print_method <- function(print_method){
  structure(print_method,
            class = c("dust_print_method", "dust_bunny"))
}
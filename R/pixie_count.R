#' @name pixie_count
#' @title Access and manipulate table numbers counters
#' 
#' @description While LaTeX provides the ability to automatically number tables, this 
#' functionality is not readily available with console, HTML, or Word output.  By 
#' keep track of the number of (captioned) tables, we can mimic the behavior of 
#' LaTeX tables to provide (mostly) consistent table numbering between formats.  The 
#' table numbering is stored in the \code{pixie_count} option.
#' 
#' @param value The value at which to set the pixie counter.
#' @param increment The value to add to the current pixie count.  Defaults to 1.
#' 
#' @details The pixie count is stored in the options and may also be accessed using
#' \code{getOption("pixie_count")}.  
#' 
#' \code{get_pixie_count} returns the current value of the counter.
#' 
#' \code{set_pixie_count} sets the value to the user-specification.
#' 
#' \code{increment_pixie_count} increments the pixie count, usually by 1.  This is called
#'   within \code{print.dust} any time a \code{dust} object has a caption.
#'   
#' @author Benjamin Nutter
#' 
#' @source 
#' The concept for these functions is loosely based on a hook meant to work with 
#' \code{knitr} to automatically number tables. 
#' http://stackoverflow.com/a/18672268/1017276
#'   
#' @export

get_pixie_count <- function()
{
  getOption("pixie_count")
}

#' @rdname pixie_count
#' @export

set_pixie_count <- function(value)
{
  checkmate::assertIntegerish(value)
  options(pixie_count = as.integer(value))
}

#' @rdname pixie_count
#' @export

increment_pixie_count <- function(increment = 1)
{
  checkmate::assertIntegerish(increment)
  options(pixie_count = getOption("pixie_count") + as.integer(increment))
}
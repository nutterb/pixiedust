#' @rdname sprinkle
#' @param print_method A character string giving the print method for the table.  
#' @export
sprinkle_print_method <- function(x, print_method = c("console", "markdown", "html", "latex"))
{
  Check <- ArgumentCheck::newArgCheck()
  if (class(x) != "dust")
    ArgumentCheck::addError(
      msg = "Sprinkles may only be added to objects of class 'dust'",
      argcheck = Check)
  
  print_method <- ArgumentCheck::match_arg(print_method,
                                           c("console", "markdown", "html", "latex"),
                                           argcheck = Check)
  ArgumentCheck::finishArgCheck(Check)
  
  x$print_method <- print_method
  x
}
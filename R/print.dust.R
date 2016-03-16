#' @name print.dust
#' @export 
#' @method print dust
#' 
#' @title Print A \code{dust} Table
#' @details Apply the formatting to a \code{dust} object and print the table.
#' 
#' @param x An object of class \code{dust}
#' @param ... Additional arguments to pass to the print method.  Currently ignored.
#' @param asis A logical value that controls if the output is printed using
#'   \code{knitr::asis_output}.  See Details.
#' 
#' @details The printing format is drawn from \code{options()$dustpan_output} and may take any of
#'   the values \code{"console"}, \code{"markdown"}, \code{"html"}, or \code{"latex"}
#'   
#'   The markdown, html, and latex output is returned via \code{\link[knitr]{asis_output}},
#'   which forces the output into the 'asis' environment.  It is intended to work 
#'   with Rmarkdown, and the tables will be rended regardless of the 
#'   chunk's \code{results} argument.  Currently, there is no way to to capture
#'   the code for additional post processing.
#'   
#'   When \code{asis = TRUE} (the default), the output is returned via \code{knitr::asis_output},
#'   which renders the output as if the chunk options included \code{results = 'asis'}.  Under 
#'   this setting, the table will be rendered regardless of the value of the \code{results} 
#'   option.  Using \code{asis = FALSE} returns a character string with the code for the table.
#'   This may be rendered in a markdown document via \code{cat(print(x, asis = FALSE))} with the 
#'   chunk option \code{results = 'asis'}.  (If working with an Rnw file, the chunk option is 
#'   \code{results = tex}).  The only way to use the \code{asis} argument is with an explicit
#'   call to \code{print.dust}.
#'   
#'   
#' @author Benjamin Nutter
#' 
#' @examples 
#' dust(lm(mpg ~ qsec + factor(am), data = mtcars))

print.dust <- function(x, ..., asis = TRUE)
{
  Check <- ArgumentCheck::newArgCheck()
  
  if (!is.null(x$caption) & !x$float & !x$longtable & x$print_method == "latex")
  {
    ArgumentCheck::addWarning(
      msg = paste0("You have requested a caption in a non-floating environment; ",
                   "the caption will be ignored \n  ",
                   "Either change set 'float = TRUE' or 'longtable = TRUE' in 'dust'"),
      argcheck = Check)
    x$caption <- NULL
  }
                   
  ArgumentCheck::finishArgCheck(Check)
  
  print_method <- x$print_method
  if (print_method == "latex" & getOption("pixiedust_latex_hhline"))
    print_method <- "latex_hhline"
  
  switch(print_method,
        "console"      = print_dust_console(x, ..., asis = asis),
        "markdown"     = print_dust_markdown(x, ..., asis = asis),
        "html"         = print_dust_html(x, ..., asis = asis),
        "latex"        = print_dust_latex(x, ..., asis = asis),
        "latex_hhline" = print_dust_latex_hhline(x, ..., asis = asis),
        stop(paste0("'", x$print_method, "' is not an valid print method")))
}

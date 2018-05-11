#' @name print.dust
#' @export 
#' @method print dust
#' 
#' @title Print A \code{dust} Table
#' @description Apply the formatting to a \code{dust} object and print the table.
#' 
#' @param x An object of class \code{dust}
#' @param ... Additional arguments to pass to the print method.  Currently ignored.
#' @param asis A logical value that controls if the output is printed using
#'   \code{knitr::asis_output}.  See Details.
#' @param linebreak_at_end Used only in HTML tables; defines the number of 
#'   line break tags \code{</br>} appended to the end of the table in order to 
#'   generate whitespace between then end of the table and the subsequent
#'   element.  By default, two line breaks are used.
#'   
#' @details The printing format is drawn from \code{options()$dustpan_output} and may take any of
#'   the values \code{"console"}, \code{"markdown"}, \code{"html"}, or \code{"latex"}
#'   
#'   The markdown, html, and latex output is returned via \code{\link[knitr]{asis_output}},
#'   which forces the output into the 'asis' environment.  It is intended to work 
#'   with Rmarkdown, and the tables will be rendered regardless of the 
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

print.dust <- function(x, ..., asis = TRUE, linebreak_at_end = 2)
{
  print_method <- x$print_method
  if (print_method == "latex" & x$hhline)
    print_method <- "latex_hhline"
  
  switch(print_method,
        "console"      = print_dust_console(x, ..., asis = asis),
        "docx"         = print_dust_markdown(x, ..., asis = asis),
        "markdown"     = print_dust_markdown(x, ..., asis = asis),
        "html"         = print_dust_html(x, ..., asis = asis, linebreak_at_end = linebreak_at_end),
        "latex"        = print_dust_latex(x, ..., asis = asis),
        "latex_hhline" = print_dust_latex_hhline(x, ..., asis = asis),
        "slidy"        = print_dust_html(x, ..., asis = asis, linebreak_at_end = linebreak_at_end),
        stop(sprintf("'%s' is not an valid print method",
                     x[["print_method"]])))
}

#' @rdname print.dust
#' @export

print.dust_list <- function(x, ..., asis = TRUE)
{
  lapply(X = x,
         FUN = print.dust,
         asis = asis, 
         ...)
}

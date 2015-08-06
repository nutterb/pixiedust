#' @name print.dust
#' @export 
#' @method print dust
#' 
#' @title Print A \code{dust} Table
#' @details Apply the formatting to a \code{dust} object and print the table.
#' 
#' @param x An object of class \code{dust}
#' @param ... Additional arguments to pass to the print method.  Currently ignored.
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
#' @author Benjamin Nutter
#' 
#' @examples 
#' dust(lm(mpg ~ qsec + factor(am), data = mtcars))

print.dust <- function(x, ...)
{
  switch(x$print_method,
        "console" = print_dust_console(x, ...),
        "markdown" = print_dust_markdown(x, ...),
        "html" = print_dust_html(x, ...),
        # "latex" = print_dust_latex(x, ...),
        stop(paste0("'", x$print_method, "' is not an valid print method")))
}

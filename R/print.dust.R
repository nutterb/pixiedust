#' @name print.dust
#' @export 
#' @method print dust
#' 
#' @title Print A \code{dust} Table
#' @details Apply the formatting to a \code{dust} object and print the table.
#' 
#' @param x An object of class \code{dust}
#' @param ... Additional arguments to pass to the print method.  Currently ignored.
#' @param longtable Allows the user to print a table in multiple sections.  This is useful when 
#'   a table has more rows than will fit on a printed page.  Acceptable inputs are \code{FALSE},
#'   indicating that only one table is printed (default); \code{TRUE} that the table should be 
#'   split into multiple tables with the default number of rows per table (see details); or a 
#'   positive integer indicating how many rows per table to include. All other values are 
#'   interpreted as \code{FALSE}.
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
#'   When using the \code{longtable=TRUE} option, the default number of rows per table is 25 for 
#'   console, HTML, and markdown output.  For LaTeX output, the number of rows is determined by 
#'   the LaTeX \code{longtable} package's algorithm. The number of rows per table only considers 
#'   the content in the body of the table.  Consideration for the number of rows in the head and 
#'   foot are the responsibility of the user.
#'   
#'   Whenever a table is broken into multiple parts, each part retains the table head.  If any 
#'   \code{interfoot} is provided, it is appended to the bottom of each section, with the 
#'   exception of the last section.  The last section has the \code{foot} appended.
#'   
#' @author Benjamin Nutter
#' 
#' @examples 
#' dust(lm(mpg ~ qsec + factor(am), data = mtcars))

print.dust <- function(x, ..., longtable = FALSE)
{
  if (!is.logical(longtable)){
    if (is.numeric(longtable) & longtable < 1) longtable <- FALSE
    else if (!is.numeric(longtable)) longtable <- FALSE
  }

  switch(x$print_method,
        "console" = print_dust_console(x, ..., longtable = longtable),
        "markdown" = print_dust_markdown(x, ..., longtable = longtable),
        "html" = print_dust_html(x, ..., longtable = longtable),
        # "latex" = print_dust_latex(x, ..., longtable = longtable),
        stop(paste0("'", x$print_method, "' is not an valid print method")))
}

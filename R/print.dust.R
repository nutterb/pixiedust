#' @name print.dust
#' @export 
#' @importFrom dplyr mutate_
#' @importFrom dplyr select_
#' @importFrom tidyr spread_
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
#' @author Benjamin Nutter
#' 
#' @examples 
#' dust(lm(mpg ~ qsec + factor(am), data = mtcars))

print.dust <- function(x, ...)
{
  switch(x$print_method,
        "console" = print_dust_console(x, ...),
#         "markdown" = print_dust_markdown(x, ...),
#         "html" = print_dust_html(x, ...),
#         "latex" = print_dust_latex(x, ...),
        stop(paste0("'", x$print_method, "' is not an valid option")))
}



print_dust_console <- function(x, ...)
{
  numeric_classes <- c("double", "numeric")
  
  obj <- perform_function(x$obj)
  
  obj <- obj %>%
    dplyr::mutate_(
      value = ~suppressWarnings(
               ifelse(!is.na(round) & col_class %in% numeric_classes,
                      as.character(round(as.numeric(value), round)),
                      value)),
      value = ~ifelse(bold, 
                      paste0("**", value, "**"), 
                      paste0("  ", value, "  ")),
      value = ~ifelse(italic, paste0("_", value, "_"), value)) %>%
    dplyr::select_("row", "col", "value") %>%
    tidyr::spread_("col", "value") %>%
    dplyr::select_("-row")
  
  colnames(obj) <- x$col_names
  
  print(obj)
}

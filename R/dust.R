#' @name dust
#' @export dust
#' @importFrom broom tidy
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate_
#' @importFrom tidyr gather_
#' 
#' @title Prepare an Object for Printing
#' @description Generates the basic structure from which \code{dustpan} tables are formatted
#'   for printing.
#'   
#' @param object An object that has a \code{tidy} method in \code{broom}
#' @param ... Additional arguments to pass to \code{tidy}
#' 
#' @details \code{dust} is intended to work directly on objects, although matrices and data frames
#'   may be printed as well.  Formatting of the table may be done by adding additional layers.
#'   
#' @return Returns an object of class \code{dust}
#' 
#' @seealso \link[broom]{tidy}
#' 
#' @author Benjamin Nutter
#' 
#' @examples 
#' x <- dust(lm(mpg ~ qsec + factor(am), data = mtcars))
#' x

dust <- function(object, ...)
{
  if (!class(object) == "data.frame") object <- broom::tidy(object, ...)

  .col_names <- colnames(object)
  names(.col_names) <- .col_names
  
  Classes <- data.frame(col_name = .col_names,
                        col_class = vapply(object, class, "class"), 
                        stringsAsFactors=FALSE)
  
  .cell_attr <- expand.grid(row = 1:nrow(object),
                            col = 1:ncol(object),
                            fn = NA,
                            bold = FALSE,
                            italic = FALSE,
                            bg = NA,
                            round = NA,
                            left_border = FALSE,
                            right_border = FALSE,
                            top_border = FALSE,
                            bottom_border = FALSE)
  
  object <- 
    dplyr::mutate_(object, row = ~1:n()) %>%
    tidyr::gather_("col", "value", gather_cols=names(object)[!names(object) %in% "row"]) %>%
    dplyr::mutate_(col_name = ~factor(col, colnames(object)),
                   col = ~as.numeric(col_name),
                   col_name = ~as.character(col_name))
  object <- 
    dplyr::left_join(object, .cell_attr,
                     by = c("row" = "row", 
                            "col" = "col"))
  object <- 
    dplyr::left_join(object, Classes,
                     by = c("col_name" = "col_name"))
  
  structure(list(obj = object,
                 col_names = .col_names,
                 print_method = getOption("dustpan_output")),
            class = "dust")
}



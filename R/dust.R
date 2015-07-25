#' @name dust
#' @export dust
#' @importFrom broom tidy
#' @importFrom dplyr distinct
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate_
#' @importFrom dplyr select_
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

  #* Build the body attributes object
  Classes <- data.frame(col_name = colnames(object),
                        col_class = vapply(object, class, "class"), 
                        stringsAsFactors=FALSE)
  
  .cell_attr <- expand.grid(row = 1:nrow(object),
                            col = 1:ncol(object),
                            fn = NA,
                            round = NA,
                            bold = FALSE,
                            italic = FALSE,
                            halign = NA,
                            valign = NA,
                            bg = NA,
                            left_border = FALSE,
                            right_border = FALSE,
                            top_border = FALSE,
                            bottom_border = FALSE,
                            stringsAsFactors=FALSE)
  
  body <- 
    dplyr::mutate_(object, row = ~1:n()) %>%
    tidyr::gather_("col", "value", gather_cols=names(object)[!names(object) %in% "row"]) %>%
    dplyr::mutate_(col_name = ~factor(col, colnames(object)),
                   col = ~as.numeric(col_name),
                   col_name = ~as.character(col_name))
  body <- 
    dplyr::left_join(body, .cell_attr,
                     by = c("row" = "row", 
                            "col" = "col"))
  body <- 
    dplyr::left_join(body, Classes,
                     by = c("col_name" = "col_name"))
  
  #* head attributes
  head <- data.frame(col_name = colnames(object),
                     col_title = colnames(object),
                     halign = rep(NA, length(object)),
                     valign = rep(NA, length(object)),
                     stringsAsFactors = FALSE)
  head_col_class <- 
    dplyr::select_(body, "col_name", "col_class") %>%
    dplyr::distinct()
  
  head <- dplyr::left_join(head, head_col_class,
                           by = c("col_name" = "col_name"))

    
    dplyr::left_join

  structure(list(body = body,
                 head = head,
                 print_method = getOption("dustpan_output")),
            class = "dust")
}



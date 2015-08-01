#' @name dust
#' @export dust
#' @importFrom broom tidy
#' @importFrom dplyr distinct
#' @importFrom dplyr left_join
#' @importFrom dplyr mutate_
#' @importFrom dplyr select_
#' @importFrom tidyr gather_
#' 
#' @title Dust Table Construction
#' @description Dust tables consist of four primary components that are 
#'   built together to create a full table.  Namely, the \code{head}, the 
#'   \code{body}, the \code{interfoot}, and the \code{foot}.  Dust tables 
#'   also contain a \code{table_attributes} object and a \code{print_method}
#'   object.
#'   
#' @param object An object that has a \code{tidy} method in \code{broom}
#' @param glance_foot Arrange the glance statistics for the \code{foot} of the
#'   table. (Not scheduled for implementation until version 0.4.0)
#' @param tidy_df When \code{object} is an object that inherits the 
#'   \code{data.frame} class, the default behavior is to assume that the 
#'   object itself is the basis of the table.  If the summarized table is 
#'   desired, set to \code{TRUE}.
#' @param ... Additional arguments to pass to \code{tidy}
#' 
#' @details The \code{head} object describes what each column of the table
#'   represents.  By default, the head is a single row, but multi row headers
#'   may be provided.  Note that multirow headers may not render in markdown
#'   as intended, though rendering in HTML and LaTeX is fairly reliable. In 
#'   longtables (tables broken over multiple pages), the \code{head} appears
#'   at the top of each table portion.
#'   
#'   The \code{body} object gives the main body of information.  In long tables,
#'   this portion is broken into portions, ideally with one portion per page.
#'   
#'   The \code{interfoot} object is an optional table to be placed at the 
#'   bottom of longtable portions with the exception of the last portion.  A 
#'   well designed \code{interfoot} can convey to the user that the table 
#'   continues on the next page.
#'   
#'   The \code{foot} object is the table that appears at the end of the 
#'   completed table.  For model objects, it is recommended that the 
#'   \code{\link[broom]{glance}} statistics be used to display model fit 
#'   statistics.
#'   
#'   The \code{table_attributes} object stores information to apply to the 
#'   entire table.
#'   
#'   The \code{print_method} object determines how the table is rendered when 
#'   the \code{print} method is invoked.  The default is to print to the 
#'   console.
#'   
#' @return Returns an object of class \code{dust}
#'
#' @section Upcoming Developments:
#' \itemize{
#'   \item{inspect_dust }{Function to evaluate a dust object for things such as 
#'      incompatible columns (the table head might have 7 columns while the
#'      body only has 6, for example); sprinkles not supported by the print
#'      method (colored text in the console); or sprinkle selections that 
#'      may cause conflicts (hopefully this won't occur, but there is potential
#'      for problems in combining attributes in LaTeX).}
#'   \item{dust_part }{A wrapper for extracting objects from a \code{dust} 
#'      object.  This is intended to assist in building custom heads and feet.}
#' }
#' 
#' @seealso \link[broom]{tidy}
#' 
#' @author Benjamin Nutter
#' 
#' @examples 
#' x <- dust(lm(mpg ~ qsec + factor(am), data = mtcars))
#' x

#' @rdname dust
#' @export
dust <- function(object, ..., glance_foot = TRUE, tidy_df = FALSE)
{
  if (!inherits(object, "data.frame") | tidy_df) 
    object <- broom::tidy(object, ...)

  head <- as.data.frame(t(names(object)),
                        stringsAsFactors=FALSE)
  names(head) <- names(object)

  structure(list(head = component_table(head, object),
                 body = component_table(object),
                 interfoot = NULL,
                 foot = NULL,
                 table_attributes = cell_attributes_frame(1, 1),
                 border_collapse = TRUE,
                 object = object,
                 print_method = getOption("pixiedust_print_method")),
            class = "dust")
}

component_table <- function(tbl, object)
{
  if (missing(object)) object <- tbl
  Classes <- data.frame(col_name = colnames(object),
                        col_class = vapply(object, class, "class"), 
                        stringsAsFactors=FALSE)
  
  tab <- gather_tbl(tbl)
  tab <- dplyr::left_join(tab, cell_attributes_frame(nrow(tbl), ncol(tbl)),
              by = c("row" = "row", "col" = "col"))
  tab <- dplyr::left_join(tab, Classes,
              by = c("col_name" = "col_name"))
  return(tab)
}

cell_attributes_frame <- function(nrow, ncol)
{
  expand.grid(row = 1:nrow,
              col = 1:ncol,
              fn = NA,
              round = NA,
              bold = FALSE,
              italic = FALSE,
              halign = NA,
              valign = NA,
              bg = NA,
              font_color = NA,
              font_size = NA,
              font_size_units = NA,
              left_border = NA,
              right_border = NA,
              top_border = NA,
              bottom_border = NA,
              height = NA,
              height_units = NA,
              width = NA,
              width_units = NA,
              rotate_degree = NA,
              pad = NA,
              stringsAsFactors=FALSE)
}

gather_tbl <- function(tbl)
{
  dplyr::mutate_(tbl, row = ~1:n()) %>%
    tidyr::gather_("col", "value", 
                   gather_cols=names(tbl)[!names(tbl) %in% "row"]) %>%
    dplyr::mutate_(col_name = ~factor(col, colnames(tbl)),
                   col = ~as.numeric(col_name),
                   col_name = ~as.character(col_name))
}


#' @name medley_all_borders
#' @title Apply Cell Borders to All Cells in a Region
#' 
#' @description For most output, specifying a region of cells with borders
#'   on all sides is as simple as giving the sprinkle \code{border = "all"}.
#'   In LaTeX output, however, this can result in thicker than expected 
#'   vertical borders.  This medley provides a LaTeX save approach to 
#'   drawing borders on all sides without getting the double vertical 
#'   border effect.
#'   
#' @param x An object of class dust 
#' @param rows The rows over which the borders are to be drawn.
#' @param cols The cols over which the borders are to be drawn.
#' @param horizontal Logical.  Toggles horizontal borders.
#' @param vertical Logical. Toggles vertical borders
#' @param part A character vector.  May contain any of \code{"body", 
#'   "head", "interfoot", "foot", "table"}.  When any element is 
#'   \code{"table"}, the borders are drawn in all parts of the table.
#'   
#' @author Benjamin Nutter
#'   
#' @export

medley_all_borders <- function(x, rows=NULL, cols=NULL, 
                               horizontal = TRUE, vertical = TRUE,
                               part = "body")
{
  checkmate::assertClass(x, 
                         classes = "dust")
  
  part <- part <- 
    match.arg(part, 
              c("table", "head", "body", "interfoot", "foot"),
              several.ok = TRUE)
  if ("table" %in% part)
  {
    part <- c("head", "body", "interfoot", "foot")
  }
  
  for (p in part)
  {
    if (!is.null(x[[p]]))
    {
      part_rows <- if (is.null(rows)) 1:max(x[[p]][["row"]]) else rows
      part_cols <- if (is.null(cols)) 1:max(x[[p]][["col"]]) else cols
      
      x <- sprinkle(x, 
                    rows = part_rows,
                    cols = part_cols,
                    border = c(if (vertical) "left" else NULL, 
                               if (horizontal) "bottom" else NULL),
                    part = p)
      if (horizontal)
      {
        x <- sprinkle(x,
                      rows = utils::head(part_rows, 1),
                      cols = part_cols,
                      border = "top",
                      part = p)
      }
      if (vertical)
      {
        x <- sprinkle(x,
                      rows = part_rows,
                      cols = utils::tail(part_cols, 1),
                      border = "right",
                      part = p)
      }
    }
  }
  x
}

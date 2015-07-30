#' @name add_sprinkles
#' @export
#' @importFrom ArgumentCheck addError
#' @importFrom ArgumentCheck finishArgCheck
#' @importFrom ArgumentCheck newArgCheck
#' @method + dust
#' 
#' @title Modify Attributes of a \code{dustpan} table
#' @description Adds attributes of a plot to be rendered by the \code{print.dust} method.
#' 
#' @param x a \code{dust} object
#' @param y a \code{dust_bunny} object.
#' 
#' @section Dust Bunnies:
#' The following is a list of available functions for adding dust bunnies to a \code{dust} table.
#' This list may or may not be complete.  Not every dust bunny will impact every type of table.
#' See \code{vignette("dustbunnies")} for a tabulation of which dustbunnies are applicable to each
#' printing method.
#' 
#' \itemize{
#'   \item{\code{\link{dust_cell_halign}}}{ Assign the horizontal alignment in a cell}
#'   \item{\code{\link{dust_colnames}}}{ Change the column names for a table}
#'   \item{\code{\link{dust_bold}}}{ Bold text in cells }
#'   \item{\code{\link{dust_fn}}}{ Apply a function to values in a table}
#'   \item{\code{\link{dust_head_halign}}}{ Assign the horizontal alignment for a column}
#'   \item{\code{\link{dust_italic}}}{ Italicize text in cells }
#'   \item{\code{\link{dust_round}}}{ Round values in cells }
#' }
#' 
#' @author Benjamin Nutter
#' 



'+.dust' <- function(x, y)
{
  Check <- ArgumentCheck::newArgCheck()
  if (!inherits(y, "sprinkle"))
    ArgumentCheck::addError("'y' must be an object with class 'sprinkle'",
                            argcheck = Check)
  
  if ("print_method" %in% class(y)){
    x$print_method <- y
    return(x)
  }
  
  if ("col_names" %in% class(y))
  {
    if (!is.null(names(y)))
    {
      if (any(!names(y) %in% x$head$col_name))
      {
        bad_names <- names(y)[!names(y) %in% x$head$col_name]
        ArgumentCheck::addError(
          msg = paste0("The following variable names are not found in the dust table:\n    ",
                       paste0(bad_names, collapse=", ")),
          argcheck = Check)
      }
      ArgumentCheck::finishArgCheck(Check)
      
      x$head$value[match(names(y), x$head$col_name)] <- y
    } 
    else{
      if (length(y) != max(x$head$col))
        ArgumentCheck::addError(
          msg = paste0("colnames sprinkle must have ", max(x$head$col), " values"),
          argcheck = Check)
      ArgumentCheck::finishArgCheck(Check)
      
      x$head$value <- y
      return(x)
    }
  } 
  else{
    ArgumentCheck::finishArgCheck(Check)
    
    if (!is.null(y$sprinkles$border_collapse))
    {
      x$border_collapse <- y$sprinkles$border_collapse
      y$sprinkles$border_collapse <- NULL
    }
    
    part <- x[[y$part]]

    if (is.null(y$rows)) y$rows <- 1:max(part$row)
    if (is.null(y$cols)) y$cols <- 1:max(part$col)
  
    Cells <- expand.grid(c(list(row = y$rows,
                                col = y$cols),
                           y$sprinkles),
                         stringsAsFactors = FALSE)
  
    if ("border" %in% names(Cells))
      Cells <- dplyr::mutate_(Cells,
                              border = ~paste0(border, "_border"),
                              border_spec = ~paste0(border_thickness, border_units, " ",
                                                    border_style, " ", border_color)) %>%
        dplyr::select_("-border_thickness", "-border_units", "-border_style", "-border_color") %>%
        tidyr::spread_("border", "border_spec")
   
    replace <- vapply(1:nrow(Cells), 
                      function(r) which(part$row == Cells$row[r] & part$col == Cells$col[r]), 
                      1)
    
    part[replace, names(Cells)[-(1:2)]] <- Cells[, -(1:2), drop=FALSE]
  
    x[[y$part]] <- part
  }
  
  return(x)
    
}


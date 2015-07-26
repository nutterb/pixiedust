#' @name add_bunnies
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
  #* Not quite sure how to make this one work yet
  #   if (!"dust_bunny" %in% class(y))
  #     ArgumentCheck::addError(
  #       msg = paste0(substitute(y), " is not a dust_bunny object"),
  #       argcheck = Check)
  
  # ArgumentCheck::finishArgCheck(Check)
  dust_bunny_type <- class(y)[1]
  switch(dust_bunny_type,
         "col_names" = add_colnames(x, y, Check),
         "dust_bg_pattern" = add_bg_pattern(x, y, Check),
         "dust_bold" = add_bold(x, y, Check),
         "dust_cell_bg" = add_cell_bg(x, y, Check),
         "dust_cell_halign" = add_cell_halign(x, y, Check),
         "dust_fn" = add_fn(x, y, Check),
         "dust_head_halign" = add_head_halign(x, y, Check),
         "dust_italic" = add_italic(x, y, Check),
         "dust_print_method" = add_print_method(x, y, Check),
         "dust_round" = add_round(x, y, Check),
         "dust_font_color"= add_font_color(x, y, Check),
         stop(paste0("dust_bunny_type '", dust_bunny_type, "' not recognized.")))
}

#**********************************************************
#**********************************************************

add_bg_pattern <- function(x, y, argcheck)
{
  if (y$across == "row"){
    n_row <- max(x$body$row)
    Colors <- data.frame(row = 1:n_row,
                         color = rep(y$colors, length.out = n_row),
                         stringsAsFactors=FALSE)
    x$body <- dplyr::left_join(x$body, Colors,
                               by = c("row" = "row")) %>%
      dplyr::mutate_(bg = ~color) %>%
      dplyr::select_("-color")
  } else{
    n_col <- max(x$body$col)
    Colors <- data.frame(col = 1:n_col,
                         color = rep(y$colors, length.out = n_col),
                         stringsAsFactors=FALSE)
    x$body <- dplyr::left_join(x$body, Colors,
                               by = c("col" = "col")) %>%
      dplyr::mutate_(bg = ~color) %>%
      dplyr::select_("-color")
  }
  return(x)
}

#**********************************************************
#**********************************************************

add_bold <- function(x, y, argcheck)
{
  cell_bunny_checks(x, y, argcheck)
  
  y[["col"]] <- unique(c(y[["col"]], match(y$colname, x$head$col_name)))
  y[["col"]] <- y[["col"]][!is.na(y$col)]

  if (length(y[["row"]]) == 0) y[["row"]] <- 1:max(x$body[["row"]])
  if (length(y[["col"]]) == 0) y[["col"]] <- 1:max(x$body[["col"]])
  
  Y <- expand.grid(row = y$row,
                   col = y[["col"]])
  
  x$body$bold[x$body$row %in% Y$row & x$body[["col"]] %in% Y[["col"]]] <- y$set_bold
  
  return(x)
}

#**********************************************************
#**********************************************************

add_cell_bg <- function(x, y, argcheck)
{
  y[["col"]] <- unique(c(y[["col"]], match(y$colname, x$head$col_name)))
  y[["col"]] <- y[["col"]][!is.na(y$col)]
  
  if (length((y[["row"]])) == 0) y[["row"]] <- 1:max(x$body[["row"]])
  if (length((y[["col"]])) == 0) y[["col"]] <- 1:max(x$body[["col"]])
  
  Y <- expand.grid(row = y$row,
                   col = y[["col"]])
  
  x$body$bg[x$body$row %in% Y$row & x$body[["col"]] %in% Y[["col"]]] <- y$color
  
  return(x)
}

#**********************************************************
#**********************************************************

add_cell_halign <- function(x, y, argcheck)
{
  cell_bunny_checks(x, y, argcheck)
  
  y[["col"]] <- unique(c(y[["col"]], match(y$colname, x$head$col_name)))
  y[["col"]] <- y[["col"]][!is.na(y$col)]
  
  if (length((y[["row"]])) == 0) y[["row"]] <- 1:max(x$body[["row"]])
  if (length((y[["col"]])) == 0) y[["col"]] <- 1:max(x$body[["col"]])
  
  Y <- expand.grid(row = y$row,
                   col = y[["col"]])
  
  x$body$halign[x$body$row %in% Y$row & x$body[["col"]] %in% Y[["col"]]] <- y$halign
  
  return(x)
}

#**********************************************************
#**********************************************************

add_colnames <- function(x, y, argcheck)
{
  if (is.null(names(y))){
    if (length(y) != nrow(x$head)){
      ArgumentCheck::addError(
        msg = paste0("Unnamed column names from 'dust_colnames' should have length ", 
                     nrow(x$head), "."),
        argcheck = argcheck)
      ArgumentCheck::finishArgCheck(argcheck)
    }
    x$head$col_title <- unclass(y)
  } 
  else{
    x$head$col_title[match(names(y), x$head$col_name)] <- y
  }
  
  return(x)
}

#**********************************************************
#**********************************************************

add_fn <- function(x, y, argcheck)
{
  cell_bunny_checks(x, y, argcheck)
  
  y[["col"]] <- unique(c(y[["col"]], match(y$colname, x$head$col_name)))
  y[["col"]] <- y[["col"]][!is.na(y$col)]
  
  if (length(y[["row"]]) == 0) y[["row"]] <- 1:max(x$body[["row"]])
  if (length(y[["col"]]) == 0) y[["col"]] <- 1:max(x$body[["col"]])
  
  Y <- expand.grid(row = y$row,
                   col = y[["col"]])
  
  x$body[x$body$row %in% Y$row & x$body[["col"]] %in% Y$col, "fn"] <- 
    vapply(X = 1:nrow(Y),
           FUN = function(i) 
             x$body[x$body$row == Y$row[i] & x$body[["col"]] == Y$col[i], "fn"] <- deparse(y$fn),
           FUN.VALUE = "character")
  
  return(x)  
}

#**********************************************************
#**********************************************************

add_font_color <- function(x, y, argcheck)
{
  cell_bunny_checks(x, y, argcheck)
  
  y[["col"]] <- unique(c(y[["col"]], match(y$colname, x$head$col_name)))
  y[["col"]] <- y[["col"]][!is.na(y$col)]
  
  if (length(y[["row"]]) == 0) y[["row"]] <- 1:max(x$body[["row"]])
  if (length(y[["col"]]) == 0) y[["col"]] <- 1:max(x$body[["col"]])
  
  Y <- expand.grid(row = y$row,
                   col = y[["col"]])
  
  x$body$font_color[x$body$row %in% Y$row & x$body[["col"]] %in% Y[["col"]]] <- y$color
  
  return(x)
}

#**********************************************************
#**********************************************************

add_head_halign <- function(x, y, argcheck)
{
  if (is.null(names(y))){
    if (length(y) != nrow(x$head)){
      ArgumentCheck::addError(
        msg = paste0("Unnamed column names from 'dust_head_halign' should have length ", 
                     nrow(x$head), "."),
        argcheck = argcheck)
      ArgumentCheck::finishArgCheck(argcheck)
    }
    x$head$halign <- unclass(y)
  } 
  else{
    x$head$halign[match(names(y), x$head$col_name)] <- y
  }
  
  return(x)
}

#**********************************************************
#**********************************************************

add_italic <- function(x, y, argcheck)
{
  cell_bunny_checks(x, y, argcheck)
  
  y[["col"]] <- unique(c(y[["col"]], match(y$colname, x$head$col_name)))
  y[["col"]] <- y[["col"]][!is.na(y$col)]
  
  if (length(y[["row"]]) == 0) y[["row"]] <- 1:max(x$body[["row"]])
  if (length(y[["col"]]) == 0) y[["col"]] <- 1:max(x$body[["col"]])
  
  Y <- expand.grid(row = y$row,
                   col = y[["col"]])
  
  x$body$italic[x$body$row %in% Y$row & x$body[["col"]] %in% Y[["col"]]] <- y$set_italic
  
  return(x)
}

#**********************************************************
#**********************************************************

add_print_method <- function(x, y, argcheck)
{
  if (!is.character(y) || length(y) != 1)
    ArgumentCheck::addError(
      msg = "'print_method' must be a character string with length 1",
      argcheck = argcheck)
  
  ArgumentCheck::finishArgCheck(argcheck)
  
  x$print_method <- unclass(y)
  return(x)
}


#**********************************************************
#**********************************************************

add_round <- function(x, y, argcheck)
{
  cell_bunny_checks(x, y, argcheck)
  
  y[["col"]] <- unique(c(y[["col"]], match(y$colname, x$head$col_name)))
  y[["col"]] <- y[["col"]][!is.na(y$col)]
  
  if (length(y[["row"]]) == 0) y[["row"]] <- 1:max(x$body[["row"]])
  if (length(y[["col"]]) == 0) y[["col"]] <- 1:max(x$body[["col"]])
  
  Y <- expand.grid(row = y$row,
                   col = y[["col"]])
  
  x$body$round[x$body$row %in% Y$row & x$body[["col"]] %in% Y[["col"]]] <- y$round
  
  return(x)
}


#**********************************************************
#**********************************************************
#* Perform the checks for valid rows and columns
#**********************************************************
#**********************************************************

cell_bunny_checks <- function(x, y, argcheck)
{
  if (!is.null(y$colname)){
    if (any(!y$colname %in% x$head$col_name)){
      mismatched <- y$colname[!y$colname %in% x$head$col_name]
      ArgumentCheck::addError(
        msg = paste0("Submitted column names could not be found: ",
                     paste0(mismatched, collapse=", ")),
        argcheck = argcheck)
    }
  }
  
  if (any(!y$row %in% x$body$row)){
    row_out_of_bounds <- y$row[!y$row %in% x$body$row]
    ArgumentCheck::addError(
      msg = paste0("There are ", max(x$body$row), " rows in the table. ",
                   "These row numbers from `dust_fn` are out of bound: ",
                   paste0(row_out_of_bounds, collapse=", ")),
      argcheck = argcheck)
  }
  
  if (any(!y[["col"]] %in% x$body[["col"]])){
    col_out_of_bounds <- y[["col"]][!y[["col"]] %in% x$body[["col"]]]
    ArgumentCheck::addError(
      msg = paste0("There are ", max(x$body[["col"]]), " columns in the table. ",
                   "These column numbers from `dust_fn` are out of bound: ",
                   paste0(row_out_of_bounds, collapse=", ")),
      argcheck = argcheck)
  }
  
  ArgumentCheck::finishArgCheck(argcheck)
}
  


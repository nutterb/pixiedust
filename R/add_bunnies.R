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
#' This list may or may not be complete.
#' \itemize{
#'   \item{\code{\link{dust_colnames}}}{ Change the column names for a table}
#'   \item{\code{\link{dust_bold}}}{ Bold text in cells }
#'   \item{\code{\link{dust_fn}}}{ Apply a function to values in a table}
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
         "dust_bold" = add_bold(x, y, Check),
         "dust_fn" = add_fn(x, y, Check),
         "dust_italic" = add_italic(x, y, Check),
         "dust_round" = add_round(x, y, Check),
         stop(paste0("dust_bunny_type '", dust_bunny_type, "' not recognized.")))
}

#**********************************************************
#**********************************************************

add_colnames <- function(x, y, argcheck)
{
  old_names <- x$col_names
  vec_names <- names(x$col_names)
  
  if (is.null(names(y))){
    if (length(y) != length(old_names)){
      ArgumentCheck::addError(
        msg = paste0("Unnamed column names from 'dust_colnames' should have length ", 
                     length(old_names), "."),
        argcheck = argcheck)
      ArgumentCheck::finishArgCheck(argcheck)
    }
    x$col_names <- unclass(y)
  } 
  else{
    x$col_names[match(names(y), old_names)] <- y
  }
  
  names(x$col_names) <- vec_names
  
  return(x)
}
  
#**********************************************************
#**********************************************************

add_bold <- function(x, y, argcheck)
{
  cell_bunny_checks(x, y, argcheck)
  
  y[["col"]] <- unique(c(y[["col"]], match(y$colname, names(x$col_name))))
  y[["col"]] <- y[["col"]][!is.na(y$col)]

  if (is.null(y[["row"]])) y[["row"]] <- 1:max(x$obj[["row"]])
  if (is.null(y[["col"]])) y[["col"]] <- 1:max(x$obj[["col"]])
  
  Y <- expand.grid(row = y$row,
                   col = y[["col"]])
  
  x$obj$bold[x$obj$row %in% Y$row & x$obj[["col"]] %in% Y[["col"]]] <- y$set_bold
  
  return(x)
}

#**********************************************************
#**********************************************************

add_fn <- function(x, y, argcheck)
{
  cell_bunny_checks(x, y, argcheck)
  
  y[["col"]] <- unique(c(y[["col"]], match(y$colname, names(x$col_name))))
  y[["col"]] <- y[["col"]][!is.na(y$col)]
  
  if (is.null(y[["row"]])) y[["row"]] <- 1:max(x$obj[["row"]])
  if (is.null(y[["col"]])) y[["col"]] <- 1:max(x$obj[["col"]])
  
  Y <- expand.grid(row = y$row,
                   col = y[["col"]])
  
  x$obj[x$obj$row %in% Y$row & x$obj[["col"]] %in% Y$col, "fn"] <- 
    vapply(X = 1:nrow(Y),
           FUN = function(i) 
             x$obj[x$obj$row == Y$row[i] & x$obj[["col"]] == Y$col[i], "fn"] <- deparse(y$fn),
           FUN.VALUE = "character")
  
  return(x)  
}

#**********************************************************
#**********************************************************

add_italic <- function(x, y, argcheck)
{
  cell_bunny_checks(x, y, argcheck)
  
  y[["col"]] <- unique(c(y[["col"]], match(y$colname, names(x$col_name))))
  y[["col"]] <- y[["col"]][!is.na(y$col)]
  
  if (is.null(y[["row"]])) y[["row"]] <- 1:max(x$obj[["row"]])
  if (is.null(y[["col"]])) y[["col"]] <- 1:max(x$obj[["col"]])
  
  Y <- expand.grid(row = y$row,
                   col = y[["col"]])
  
  x$obj$italic[x$obj$row %in% Y$row & x$obj[["col"]] %in% Y[["col"]]] <- y$set_italic
  
  return(x)
}

#**********************************************************
#**********************************************************

add_round <- function(x, y, argcheck)
{
  cell_bunny_checks(x, y, argcheck)
  
  y[["col"]] <- unique(c(y[["col"]], match(y$colname, names(x$col_name))))
  y[["col"]] <- y[["col"]][!is.na(y$col)]
  
  if (is.null(y[["row"]])) y[["row"]] <- 1:max(x$obj[["row"]])
  if (is.null(y[["col"]])) y[["col"]] <- 1:max(x$obj[["col"]])
  
  Y <- expand.grid(row = y$row,
                   col = y[["col"]])
  
  x$obj$round[x$obj$row %in% Y$row & x$obj[["col"]] %in% Y[["col"]]] <- y$round
  
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
    if (any(!y$colname %in% names(x$col_name))){
      mismatched <- y$colname[!y$colname %in% names(x$col_name)]
      ArgumentCheck::addError(
        msg = paste0("Submitted column names could not be found: ",
                     paste0(mismatched, collapse=", ")),
        argcheck = argcheck)
    }
  }
  
  if (any(!y$row %in% x$obj$row)){
    row_out_of_bounds <- y$row[!y$row %in% x$obj$row]
    ArgumentCheck::addError(
      msg = paste0("There are ", max(x$obj$row), " rows in the table. ",
                   "These row numbers from `dust_fn` are out of bound: ",
                   paste0(row_out_of_bounds, collapse=", ")),
      argcheck = argcheck)
  }
  
  if (any(!y[["col"]] %in% x$obj[["col"]])){
    col_out_of_bounds <- y[["col"]][!y[["col"]] %in% x$obj[["col"]]]
    ArgumentCheck::addError(
      msg = paste0("There are ", max(x$obj[["col"]]), " columns in the table. ",
                   "These column numbers from `dust_fn` are out of bound: ",
                   paste0(row_out_of_bounds, collapse=", ")),
      argcheck = argcheck)
  }
  
  ArgumentCheck::finishArgCheck(argcheck)
}
  


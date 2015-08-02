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
#' @seealso \code{\link{dust}}, \code{\link{sprinkle}}
#' 
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
    
    if (!is.null(y$sprinkles[["bg_pattern"]])){
      x$bg_pattern <- y$sprinkles$bg_pattern
      x$bg_pattern_by <- y$sprinkles$bg_pattern_by
      
      bg_pattern <- y$sprinkles$bg_pattern
      pattern_by <- y$sprinkles$bg_pattern_by
      
      y$sprinkles$bg_pattern <- NULL
      y$sprinkles$bg_pattern_by <- NULL
    }
    
    part <- x[[y$part]]
    
    if (!is.null(y$cols)){
      cols_num <- suppressWarnings(as.numeric(y$cols))
      cols_num <- cols_num[!is.na(cols_num)]
      
      cols_str <- match(y$cols, unique(x$head$col_name))
      y$cols <- unique(c(cols_num, cols_str))
      y$cols <- y$cols[!is.na(y$cols)]
    }

    if (is.null(y$rows) | length(y$rows) == 0) y$rows <- 1:max(part$row)
    if (is.null(y$cols) | length(y$cols) == 0) y$cols <- 1:max(part$col)
  
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
    
    if (exists("bg_pattern")){
      if (pattern_by == "rows"){
        bg_frame <- dplyr::data_frame(row = unique(Cells$row))
        bg_frame[["bg"]] <- rep(bg_pattern, length.out = nrow(bg_frame))
        Cells <- dplyr::left_join(Cells, bg_frame, by = c("row" = "row"))
        if ("bg.x" %in% names(Cells)){
          Cells <- dplyr::rename_(Cells, "bg" = "bg.y") %>%
            dplyr::select_("-bg.x")
        }
      }
      else {
        bg_frame <- dplyr::data_frame(col = unique(Cells$col))
        bg_frame[["bg"]] <- rep(bg_pattern, length.out = nrow(bg_frame))
        Cells <- dplyr::left_join(Cells, bg_frame, by = c("col" = "col"))
        if ("bg.x" %in% names(Cells)){
          Cells <- dplyr::rename_(Cells, "bg" = "bg.y") %>%
            dplyr::select_("-bg.x")
        }
      }
    }
   
    replace <- vapply(1:nrow(Cells), 
                      function(r) which(part$row == Cells$row[r] & part$col == Cells$col[r]), 
                      1)
    
    part[replace, names(Cells)[-(1:2)]] <- Cells[, -(1:2), drop=FALSE]
  
    x[[y$part]] <- part
  }
  
  return(x)
    
}


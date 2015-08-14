#' @rdname dust
#' 
#' @param x A dust object
#' @param table A data frame of similar dimensions of the part being replaced.
#' @param part The part of the table to replace with \code{table}
#' 
#' @export 
redust <- function(x, table, part = c("head", "foot", "interfoot", "body")){
  Check <- ArgumentCheck::newArgCheck()
  
  #* x must have class 'dust'
  if (class(x) != "dust")
    ArgumentCheck::addError(
      msg = "Sprinkles may only be added to objects of class 'dust'",
      argcheck = Check)
  
  part_str <- ArgumentCheck::match_arg(part, c("head", "foot",
                                               "interfoot", "body"),
                                       argcheck = Check)
  
  
  
  colCounts <- vapply(x[c("head", "body", "foot", "interfoot")], 
                      col_count, 1)
  n_colCounts <- unique(colCounts[!is.na(colCounts)])
  
  if (length(n_colCounts) > 1){
      ArgumentCheck::addError(
        msg = paste0("All parts of the table must have the same number of columns (or none).\n", 
                     "    Currently: ", 
                     paste0(paste0(c("head", "body", "foot", "interfoot"), " (", colCounts, ")"),
                            collapse = ", ")),
        argcheck = Check)
  }
  
  if (!all(colCounts[!is.na(colCounts)] %in% ncol(table)))
    ArgumentCheck::addError(
      msg = paste0("The current table has ", paste0(n_colCounts, collapse = "/"), " columns and you ",
                   "are attempting to impose a part\n",
                   "    with ", ncol(table), " columns."),
      argcheck = Check)
  
  ArgumentCheck::finishArgCheck(Check)
  
  col_name_class <- dplyr::filter_(x$head, "row == 1") %>%
    dplyr::select_("row", "col", "col_name", "col_class") %>%
    dplyr::arrange_("row", "col")
  
  part <- component_table(table)
  part$col_name <- rep(col_name_class$col_name, each = nrow(table))
  part$col_class <- rep(col_name_class$col_class, each = nrow(table))

  x[[part_str]] <- part
  
  x
}

#*****

col_count <- function(p){
  if (is.null(p)) return(NA) else return(max(p$col))
}
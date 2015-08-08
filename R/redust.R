#' @rdname dust
#' 
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
  
  if (length(part_str) > 0){
    part <- x[[part_str]]
  
    ideal_cols <- max(part$col)
  
    if (ncol(table) != max(part$col))
      ArgumentCheck::addError(
        msg = paste0("The existing table has ", max(part$col), 
                     " while your replacement table has ", ncol(table)),
        argcheck = Check)
  }
  
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
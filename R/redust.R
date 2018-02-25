#' @rdname dust
#' 
#' @param x A dust object
#' @param table A data frame of similar dimensions of the part being replaced.
#' @param part The part of the table to replace with \code{table}

#' @rdname dust
#' @export 

redust <- function(x, table, part = c("head", "foot", "interfoot", "body"))
{
  UseMethod("redust")
}

#' @rdname dust
#' @export

redust.default <- function(x, table, part = c("head", "foot", "interfoot", "body"))
{
  coll <- checkmate::makeAssertCollection()

  #* x must have class 'dust'
  checkmate::assertClass(x = x,
                         classes = "dust",
                         add = coll)

  part_str <- checkmate::matchArg(x = part, 
                               choices = c("head", "foot",
                                           "interfoot", "body"),
                               add = coll)
  
  
  
  colCounts <- vapply(x[c("head", "body", "foot", "interfoot")], 
                      col_count, 1)
  n_colCounts <- unique(colCounts[!is.na(colCounts)])
  
  if (length(n_colCounts) > 1){
      coll$push(
        paste0("All parts of the table must have the same number of columns (or none).\n", 
               "    Currently: ", 
               paste0(paste0(c("head", "body", "foot", "interfoot"), " (", colCounts, ")"),
                      collapse = ", "))
      )
  }
  
  if (!all(colCounts[!is.na(colCounts)] %in% ncol(table)))
    coll$push(
      paste0("The current table has ", paste0(n_colCounts, collapse = "/"), " columns and you ",
             "are attempting to impose a part\n",
             "    with ", ncol(table), " columns.")
    )
  
  checkmate::reportAssertions(coll)
  
  col_name_class <- 
    x[["head"]][x[["head"]][["row"]] == 1, ]
  col_name_class <- col_name_class[c("row", "col", "col_name", "col_class")]
  col_name_class <- col_name_class[order(col_name_class[["row"]],
                                         col_name_class[["col"]]), ]
  
  part <- component_table(table)
  part$col_name <- rep(col_name_class$col_name, each = nrow(table))
  part$col_class <- rep(col_name_class$col_class, each = nrow(table))

  x[[part_str]] <- part
  
  x
}

#' @rdname dust
#' @export

redust.dust_list <- function(x, table, part = c("head", "foot", "interfoot", "body"))
{
  structure(
    lapply(X = x,
           FUN = redust.default,
           table = table,
           part = part),
    class = "dust_list"
  )
}

#*****

col_count <- function(p){
  if (is.null(p)) return(NA) else return(max(p$col))
}
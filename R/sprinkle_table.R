#' @rdname sprinkle
#' @export

sprinkle_table <- function(x, cols = NULL, ..., part = "table")
{
  UseMethod("sprinkle_table")
}

#' @rdname sprinkle
#' @export

sprinkle_table.default <- function(x, cols=NULL, ..., 
                                   part = "table"){

  part_names <- checkmate::matchArg(x = part, 
                                 choices = c("table", "body", "head", "foot", "interfoot"),
                                 several.ok = TRUE)

  if (length(part_names) > 0)
    if (any(part_names %in% "table")) part_names <- c("body", "head", "foot", "interfoot")
  
  if ("body" %in% part_names) x <- sprinkle(x, cols = cols, ..., part = "body")
  if ("head" %in% part_names) x <- sprinkle(x, cols = cols, ..., part = "head")
  if ("foot" %in% part_names & !is.null(x$foot)) x <- sprinkle(x, cols = cols, ..., part = "foot")
  if ("interfoot" %in% part_names & !is.null(x$interfoot)) 
    x <- sprinkle(x, cols=cols, ..., part = "interfoot")
  
  return(x)
}

#' @rdname sprinkle
#' @export

sprinkle_table.dust_list <- function(x, cols=NULL, ..., 
                                     part = "table")
{
  structure(
    lapply(X = x,
           FUN = sprinkle_table,
           cols = cols,
           part = part,
           ...),
    class = "dust_list"
  )
}

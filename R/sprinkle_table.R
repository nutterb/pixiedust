#' @rdname sprinkle
#' @export
sprinkle_table <- function(x, cols=NULL, ..., 
                           part = "table"){
  Check <- ArgumentCheck::newArgCheck()
  
  part_names <- ArgumentCheck::match_arg(part, 
                                         c("table", "body", "head", "foot", "interfoot"),
                                         several.ok = TRUE,
                                         argcheck = Check)
  
  ArgumentCheck::finishArgCheck(Check)
  
  if (length(part_names) > 0)
    if (any(part_names %in% "table")) part_names <- c("body", "head", "foot", "interfoot")
  
  if ("body" %in% part_names) x <- sprinkle(x, cols = cols, ..., part = "body")
  if ("head" %in% part_names) x <- sprinkle(x, cols = cols, ..., part = "head")
  if ("foot" %in% part_names & !is.null(x$foot)) x <- sprinkle(x, cols = cols, ..., part = "foot")
  if ("interfoot" %in% part_names & !is.null(x$interfoot)) 
    x <- sprinkle(x, cols=cols, ..., part = "interfoot")
  
  return(x)
}
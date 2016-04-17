#' @rdname sprinkle
#' @param print_method A character string giving the print method for the table. 
#'   Note: \code{"docx"} is synonymous with \code{"markdown"}.  
#' @export

sprinkle_print_method <- function(x, 
                                  print_method = c("console", "markdown", "html", "latex"))
{
  UseMethod("sprinkle_print_method")
}

#' @rdname sprinkle
#' @export

sprinkle_print_method.default <- function(x, 
                                          print_method = c("console", "markdown", 
                                                           "html", "latex", "docx"))
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assertClass(x, 
                         classes = "dust",
                         add = coll)
  
  checkmate::assertSubset(print_method,
                          c("console", "markdown", "html", "latex", "docx"),
                          add = coll)

  checkmate::reportAssertions(coll)

  x[["print_method"]] <- print_method
  x
}

#' @rdname sprinkle
#' @export

sprinkle_print_method.dust_list <- function(x, 
                                          print_method = c("console", "markdown", 
                                                           "html", "latex"))
{
  structure(
    lapply(X = x,
           FUN = sprinkle_print_method,
           print_method = print_method),
    class = "dust_list"
  )
}

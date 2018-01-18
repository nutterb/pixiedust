#' @name knit_print.dust
#' @title \code{knitr} Printing Function 
#' 
#' @description Custom printing functions for displaying \code{dust} and 
#'   \code{dust_list} objects in R Markdown documents.
#'   
#' @param x A dust object
#' @param options A list of options received from the chunk options.
#' @param ... Additional arguments to pass to other methods.
#' 
#' @export

knit_print.dust <- function(x, options, ...){
  if (missing(options)) options <- list()
  print.dust(x, 
             ..., 
             asis = TRUE, 
             interactive = is.null(options$results))
}

#' @rdname knit_print.dust
#' @export

knit_print.dust_list <- function(x, options, ...){
  if (missing(options)) options <- list()
  print.dust_list(x, 
                  ..., 
                  asis = TRUE,
                  interactive = is.null(options$results))
}
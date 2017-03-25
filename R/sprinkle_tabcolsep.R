#' @name sprinkle_tabcolsep
#' @title Change the tabcolsep Property in a Dust Table
#' 
#' @description The \code{tabcolsep} property controls the space between 
#'   columns in LaTeX output.  By default, it is set to 6 pt.
#'   
#' @param x An object of class \code{dust}
#' @param tabcolsep \code{numeric(1)}, integer-like value.
#' @param ... Additional arguments to pass to other methods. Currently ignored.
#'
#' @details Reading on the details of \code{tabcolsep} may be done by 
#'   searching "latex tabcolsep" on the internet.
#' 
#' This property has no effect on non-LaTeX output.
#'   
#' @author Benjamin Nutter
#' 
#' @source \url{https://www.google.com/webhp?sourceid=chrome-instant&rlz=1C1CHBF_enUS706US706&ion=1&espv=2&ie=UTF-8#q=latex+tabcolsep&*}
#' 
#' @seealso \code{\link{dust}}, \code{\link{sprinkle}}
#' 
#' @section Functional Requirements:
#' \enumerate{
#'  \item Change the \code{tabcolsep} attribute of the \code{dust} object.
#'  \item Cast an error if \code{x} is not a \code{dust} object.
#'  \item Cast an error if \code{tabcolsep} is not integerish and length 1.
#' }
#' 
#' @export

sprinkle_tabcolsep <- function(x, 
                               tabcolsep = getOption("pixie_tabcolsep", 6), 
                               ...)
{
  UseMethod("sprinkle_tabcolsep")
}

#' @rdname sprinkle_tabcolsep
#' @export

sprinkle_tabcolsep.default <- function(x, 
                                       tabcolsep = getOption("pixie_tabcolsep", 6), 
                                       ...)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = x,
                          classes = "dust",
                          add = coll)
  
  checkmate::assert_integerish(x = tabcolsep,
                               len = 1,
                               add = coll)
  
  checkmate::reportAssertions(coll)
  
  x[["tabcolsep"]] <- tabcolsep
  
  x
}

#' @rdname sprinkle_tabcolsep
#' @export

sprinkle_tabcolsep.dust_list <- function(x, 
                                         tabcolsep = getOption("pixie_tabcolsep", 6),
                                         ...)
{
  lapply(x,
         sprinkle_tabcolsep.default,
         tabcolsep)
}
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
  
  sprinkle_tabcolsep_index_assert(tabcolsep = tabcolsep, 
                                  coll = coll)
  
  checkmate::reportAssertions(coll)
  
  sprinkle_tabcolsep_index(x = x, 
                           tabcolsep = tabcolsep)
}

#' @rdname sprinkle_tabcolsep
#' @export

sprinkle_tabcolsep.dust_list <- function(x, 
                                         tabcolsep = getOption("pixie_tabcolsep", 6),
                                         ...)
{
  structure(
    lapply(x,
           sprinkle_tabcolsep.default,
           tabcolsep),
    class = "dust_list"
  )
}

# Unexported utilities ----------------------------------------------
# These functions carry the the `_index` suffix for consistency with 
# the cell-valued sprinkles, but they don't actually require an 
# index, since they change table-valued sprinkles

sprinkle_tabcolsep_index_assert <- function(tabcolsep = getOption("pixie_tabcolsep", 6),
                                            coll)
{
  checkmate::assert_integerish(x = tabcolsep,
                               len = 1,
                               add = coll,
                               .var.name = "tabcolsep")
}

# indices argument is only present to avoid errors when the argument is passed 
# from sprinkle
sprinkle_tabcolsep_index <- function(x, tabcolsep, 
                                     indices = NULL, part = NULL)
{
  x[["tabcolsep"]] <- tabcolsep
  
  x
}
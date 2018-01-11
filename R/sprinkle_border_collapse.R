#' @name sprinkle_border_collapse
#' @title Change the Border Collapse Property in a Dust Table
#' 
#' @description The \code{border_collapse} property controls the appearance of 
#'   cell borders in HTML tables.  Be default, \code{pixiedust} collapses 
#'   the borders so that the adjoining border of two cells appear as a 
#'   single border.
#'   
#' @param x An object of class \code{dust}
#' @param border_collapse \code{character(1)}. Defaults to \code{"collapse"}, 
#'   and may accept any of \code{"collapse"}, \code{"separate"}, 
#'   \code{"initial"}, or \code{"inherit"}.
#' @param ... Additional arguments to pass to other methods. Currently ignored.
#' 
#' @details See \url{https://www.w3schools.com/cssref/pr_border-collapse.asp} 
#' for details on how each option affects the appearance of a table.
#' 
#' This property has no effect on non-HTML output.
#'   
#' @author Benjamin Nutter
#' 
#' @source \url{https://www.w3schools.com/cssref/pr_border-collapse.asp}
#' 
#' @seealso \code{\link{dust}}, \code{\link{sprinkle}}
#' 
#' @section Functional Requirements:
#' \enumerate{
#'  \item Change the \code{border_collapse} attribute of the \code{dust} object.
#'  \item Cast an error if \code{x} is not a \code{dust} object.
#'  \item Cast an error if \code{border_collapse} is not one of 
#'    \code{"collapse"}, \code{"separate"}, \code{"initial"}, \code{"inherit"}.
#' }
#' 
#' @export

sprinkle_border_collapse <- function(x, 
                            border_collapse = getOption("pixie_border_collapse", "collapse"), 
                            ...)
{
  UseMethod("sprinkle_border_collapse")
}

#' @rdname sprinkle_border_collapse
#' @export

sprinkle_border_collapse.default <- function(x, 
                                    border_collapse = getOption("pixie_border_collapse", "collapse"), 
                                    ...)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = x,
                          classes = "dust",
                          add = coll)
  
  border_collapse <- 
    sprinkle_border_collapse_index_assert(border_collapse = border_collapse,
                                          coll = coll)
  
  checkmate::reportAssertions(coll)
  
  sprinkle_border_collapse_index(x = x,
                                 border_collapse = border_collapse)
}

#' @rdname sprinkle_border_collapse
#' @export

sprinkle_border_collapse.dust_list <- function(x, 
                                      border_collapse = getOption("pixie_border_collapse", "collapse"),
                                      ...)
{
  structure(
    lapply(x,
           sprinkle_border_collapse.default,
           border_collapse),
    class = "dust_list"
  )
}

# Unexported utilities ----------------------------------------------
# These functions carry the the `_index` suffix for consistency with 
# the cell-valued sprinkles, but they don't actually require an 
# index, since they change table-valued sprinkles

sprinkle_border_collapse_index_assert <- function(border_collapse = getOption("pixie_border_collapse", "collapse"), 
                                                  coll)
{
  checkmate::matchArg(x = border_collapse,
                      choices = c("collapse", "separate", "initial",
                                  "inherit"),
                      add = coll,
                      .var.name = "border_collapse")
}

# indices argument is only present to avoid errors when the argument is passed 
# from sprinkle
sprinkle_border_collapse_index <- function(x, border_collapse, indices = NULL,
                                           part = NULL)
{
  x[["border_collapse"]] <- border_collapse
  
  x
}
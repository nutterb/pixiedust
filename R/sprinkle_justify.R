#' @name sprinkle_justify
#' @title Change the Caption in a Dust Table
#' 
#' @description The justification of the table determines the horizontal 
#'   placing of the table on the page.
#'   
#' @param x An object of class \code{dust}
#' @param justify \code{character} string giving the justification of the 
#'   entire table on the page. May be any one of \code{"center"},
#'   \code{"left"}, or \code{"right"}.
#' @param ... Additional arguments to pass to other methods. Currently ignored.
#'
#' @details For HTML tables, the values \code{"left"}, \code{"center"}, 
#'   and \code{"right"} all justify the table as expected.
#'   
#'   In LaTeX output, both \code{"right"} and \code{"left"} justify 
#'   to the left. This may change in the future if I find a resolution.
#'   
#' @author Benjamin Nutter
#' 
#' @seealso \code{\link{dust}}, \code{\link{sprinkle}}
#' 
#' @section Functional Requirements:
#' \enumerate{
#'  \item Change the \code{justify} attribute of the \code{dust} object.
#'  \item Cast an error if \code{x} is not a \code{dust} object.
#'  \item Cast an error if \code{justify} is not one of \code{"center"}, 
#'        \code{"left"}, or \code{"right"}.
#'  \item Ignore capitalization of the \code{justify} argument.
#' }
#' 
#' @export

sprinkle_justify <- function(x, 
                             justify = getOption("pixie_justify", "center"), 
                             ...)
{
  UseMethod("sprinkle_justify")
}

#' @rdname sprinkle_justify
#' @export

sprinkle_justify.default <- function(x, 
                                     justify = getOption("pixie_justify", "center"), 
                                     ...)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = x,
                          classes = "dust",
                          add = coll)
  
  justify <- checkmate::matchArg(x = tolower(justify),
                                 choices = c("center", "left", "right"),
                                 add = coll)
  
  checkmate::reportAssertions(coll)
  
  x[["justify"]] <- justify
  
  x
}

#' @rdname sprinkle_justify
#' @export

sprinkle_justify.dust_list <- function(x, 
                                       justify = getOption("pixie_justify", "center"),
                                       ...)
{
  lapply(x,
         sprinkle_justify.default,
         justify)
}

# Unexported utilities ----------------------------------------------
# These functions carry the the `_index` suffix for consistency with 
# the cell-valued sprinkles, but they don't actually require an 
# index, since they change table-valued sprinkles

sprinkle_justify_index_assert <- function(justify, coll)
{
  checkmate::matchArg(x = justify,
                      choices = c("center", "left", "right"),
                      add = coll,
                      .var.name = "justify")
}

sprinkle_justify_index <- function(x, justify)
{
  x[["justify"]] <- justify
  
  x
}
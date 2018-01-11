#' @name sprinkle_float
#' @title Change the float Property in a Dust Table
#' 
#' @description Alter the floating behavior of tables rendered in 
#' LaTeX documents.  Floating tables are moved to a position deemed ideal 
#' by the typesetter.  Setting \code{float = FALSE} causes the table to 
#' be rendered in the position in which it is generated in the code.
#'   
#' @param x An object of class \code{dust}
#' @param float \code{logical(1)} indicating if the table should be placed in
#'   a floating environment.
#' @param ... Additional arguments to pass to other methods. Currently ignored.
#'
#' @details See \url{https://en.wikibooks.org/wiki/LaTeX/Floats,_Figures_and_Captions}
#' for more about floating environments in LaTeX.
#' 
#' This property has no effect on non-LaTeX output.
#'   
#' @author Benjamin Nutter
#' 
#' @source \url{https://en.wikibooks.org/wiki/LaTeX/Floats,_Figures_and_Captions}
#' 
#' @seealso \code{\link{dust}}, \code{\link{sprinkle}}
#' 
#' @section Functional Requirements:
#' \enumerate{
#'  \item Change the \code{float} attribute of the \code{dust} object.
#'  \item Cast an error if \code{x} is not a \code{dust} object.
#'  \item Cast an error if \code{float} is not logical or length 1.
#' }
#' 
#' @export

sprinkle_float <- function(x, 
                               float = getOption("pixie_float", FALSE), 
                               ...)
{
  UseMethod("sprinkle_float")
}

#' @rdname sprinkle_float
#' @export

sprinkle_float.default <- function(x, 
                                       float = getOption("pixie_float", FALSE), 
                                       ...)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = x,
                          classes = "dust",
                          add = coll)
  
  sprinkle_float_index_assert(float = float,
                              coll = coll)
  
  checkmate::reportAssertions(coll)
  
  sprinkle_float_index(x = x,
                       float = float)
}

#' @rdname sprinkle_float
#' @export

sprinkle_float.dust_list <- function(x, 
                                         float = getOption("pixie_float", FALSE),
                                         ...)
{
  structure(
    lapply(x,
           sprinkle_float.default,
           float),
    class = "dust_list"
  )
}

# Unexported utilities ----------------------------------------------
# These functions carry the the `_index` suffix for consistency with 
# the cell-valued sprinkles, but they don't actually require an 
# index, since they change table-valued sprinkles

sprinkle_float_index_assert <- function(float = getOption("pixie_float", FALSE), coll)
{
  checkmate::assert_logical(x = float,
                            len = 1,
                            add = coll,
                            .var.name = "float")
}

# indices argument is only present to avoid errors when the argument is passed 
# from sprinkle
sprinkle_float_index <- function(x, float, indices = NULL, part = NULL)
{
  x[["float"]] <- float
  
  x
}
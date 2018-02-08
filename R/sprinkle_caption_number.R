#' @name sprinkle_caption_number
#' @title Change the Caption in a Dust Table
#' 
#' @description The table caption is often used as a brief title, but may also 
#'   be used to provide a longer statement explaining how to interpret the 
#'   table results.
#'   
#' @param x An object of class \code{dust}
#' @param caption_number \code{logical(1)} When \code{TRUE}, the table caption 
#'   is prefixed with "Table #". Table numbering is suppressed when 
#'   \code{FALSE}.  When numbering is suppressed, the table number counter
#'   will not increment.
#' @param ... Additional arguments to pass to other methods. Currently ignored.
#'
#' @details Table numbering makes it possible to reference tables within a 
#'   document.  In some cases, the numbering is not desired. Suppressing 
#'   numbering may restrict the ability to make reference to the table.
#'   
#' @author Benjamin Nutter
#' 
#' @seealso \code{\link{dust}}, \code{\link{sprinkle}}
#' 
#' @section Functional Requirements:
#' \enumerate{
#'  \item Change the \code{caption_number} attribute of the \code{dust} object.
#'  \item Cast an error if \code{x} is not a \code{dust} object.
#'  \item Cast an error if \code{caption_number} is not a logical object.
#'  \item Cast an error if \code{caption_number} has length greater than 1.
#' }
#' 
#' @export

sprinkle_caption_number <- function(x, caption_number, ...)
{
  UseMethod("sprinkle_caption_number")
}

#' @rdname sprinkle_caption_number
#' @export

sprinkle_caption_number.default <- function(x, 
                                            caption_number = getOption("pixie_caption_number", TRUE), 
                                            ...)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = x,
                          classes = "dust",
                          add = coll)
  
  sprinkle_caption_number_index_assert(caption_number = caption_number,
                                       coll = coll)
  
  checkmate::reportAssertions(coll)
  
  sprinkle_caption_number_index(x = x,
                                caption_number = caption_number)
}

#' @rdname sprinkle_caption_number
#' @export

sprinkle_caption_number.dust_list <- function(x, 
                                              caption_number = getOption("pixie_caption_number", TRUE), 
                                              ...)
{
  structure(
    lapply(x,
           sprinkle_caption_number.default,
           caption_number),
    class = "dust_list"
  )
}

# Unexported utilities ----------------------------------------------
# These functions carry the the `_index` suffix for consistency with 
# the cell-valued sprinkles, but they don't actually require an 
# index, since they change table-valued sprinkles

sprinkle_caption_number_index_assert <- function(caption_number, coll)
{
  if (!missing(caption_number)){
    checkmate::assert_logical(x = caption_number,
                                len = 1,
                                add = coll,
                                .var.name = "caption_number")
  }
}

# indices argument is only present to avoid errors when the argument is passed 
# from sprinkle
sprinkle_caption_number_index <- function(x, caption_number = getOption("pixie_caption_number", TRUE), 
                                   indices = NULL, part = NULL)
{
  x[["caption_number"]] <- caption_number
  
  x
}
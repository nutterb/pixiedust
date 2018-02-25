#' @name sprinkle_hhline
#' @title Change the hhline Property in a Dust Table
#' 
#' @description The \code{hhline} property controls the appearance of 
#'   cell borders in LaTeX tables.  There is a known limitation in the 
#'   LaTeX \code{colortbl} package where cell borders can be hidden if
#'   the cell has a background color.  If using both cell borders and
#'   background colors, it is recommended that you use the \code{hhline}
#'   property to make cell borders appear as desired.
#'   
#' @param x An object of class \code{dust}
#' @param hhline \code{logical(1)}. When \code{TRUE}, the LaTeX hhline package
#'   will be used for cell borderes.
#' @param ... Additional arguments to pass to other methods. Currently ignored.
#' 
#' @details When \code{hhline = TRUE}, borders will be solid; dashed and dotted
#'   borders are unsupported by hhline.
#' 
#' This property has no effect on non-LaTeX output.
#'   
#' @author Benjamin Nutter
#' 
#' @source \url{https://www.ctan.org/pkg/hhline?lang=en}
#' 
#' @seealso \code{\link{dust}}, \code{\link{sprinkle}}
#' 
#' @section Functional Requirements:
#' \enumerate{
#'  \item Change the \code{hhline} attribute of the \code{dust} object.
#'  \item Cast an error if \code{x} is not a \code{dust} object.
#'  \item Cast an error if \code{hhline} is not logical and length 1.
#' }
#' 
#' @export

sprinkle_hhline <- function(x, 
                            hhline = getOption("pixie_hhline", FALSE), 
                            ...)
{
  UseMethod("sprinkle_hhline")
}

#' @rdname sprinkle_hhline
#' @export

sprinkle_hhline.default <- function(x, 
                                    hhline = getOption("pixie_hhline", FALSE), 
                                    ...)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = x,
                          classes = "dust",
                          add = coll)
  
  sprinkle_hhline_index_assert(hhline = hhline,
                               coll = coll)
  
  checkmate::reportAssertions(coll)
  
  sprinkle_hhline_index(x = x,
                        hhline = hhline)
}

#' @rdname sprinkle_hhline
#' @export

sprinkle_hhline.dust_list <- function(x, 
                                      hhline = getOption("pixie_hhline", FALSE),
                                      ...)
{
  structure(
    lapply(x,
           sprinkle_hhline.default,
           hhline),
    class = "dust_list"
  )
}

# Unexported utilities ----------------------------------------------
# These functions carry the the `_index` suffix for consistency with 
# the cell-valued sprinkles, but they don't actually require an 
# index, since they change table-valued sprinkles

sprinkle_hhline_index_assert <- function(hhline = getOption("pixie_hhline", FALSE),
                                         coll)
{
  checkmate::assert_logical(x = hhline,
                            len = 1,
                            add = coll,
                            .var.name = "hhline")
}

# indices argument is only present to avoid errors when the argument is passed 
# from sprinkle
sprinkle_hhline_index <- function(x, hhline, indices = NULL, part = NULL)
{
  x[["hhline"]] <- hhline
  
  x
}
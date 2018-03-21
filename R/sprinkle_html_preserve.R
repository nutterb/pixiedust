#' @name sprinkle_html_preserve
#' @title Change the HTML Preserve Property in a Dust Table
#' 
#' @description By default \code{pixiedust} makes use of \code{htmltools::htmlPreserve}
#'   to prevent certain symbols from rendering in unintended ways based on some 
#'   not-very-well-understood-by-the-author issues.  This property controls whether
#'   the preservation is used or not.
#'   
#' @param x An object of class \code{dust}
#' @param html_preserve \code{logical(1)} indicating if the table is being produced
#'   in a \code{htmltools::htmlPreserve} environment.
#' @param ... Additional arguments to pass to other methods. Currently ignored.
#'   
#' @author Benjamin Nutter
#' 
#' @seealso \code{\link{dust}}, \code{\link{sprinkle}}, \code{\link[htmltools]{htmlPreserve}}
#' 
#' @section Functional Requirements:
#' \enumerate{
#'  \item Change the \code{html_preserve} attribute of the \code{dust} object.
#'  \item Cast an error if \code{x} is not a \code{dust} object.
#'  \item Cast an error if \code{html_preserve} is not logical(1).
#' }
#' 
#' @export

sprinkle_html_preserve <- function(x, 
                                   html_preserve = getOption("pixie_html_preserve", TRUE), 
                                   ...)
{
  UseMethod("sprinkle_html_preserve")
}

#' @rdname sprinkle_html_preserve
#' @export

sprinkle_html_preserve.default <- function(x, 
                                           html_preserve = getOption("pixie_html_preserve", TRUE),
                                           ...)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = x,
                          classes = "dust",
                          add = coll)
  
  sprinkle_html_preserve_index_assert(html_preserve = html_preserve,
                                      coll = coll)
  
  checkmate::reportAssertions(coll)

  sprinkle_html_preserve_index(x = x,
                               html_preserve = html_preserve)
}

#' @rdname sprinkle_html_preserve
#' @export

sprinkle_html_preserve.dust_list <- function(x, 
                                             html_preserve = getOption("pixie_html_preserve", TRUE),
                                             ...)
{
  structure(
    lapply(x,
           sprinkle_html_preserve.default,
           html_preserve),
    class = "dust_list"
  )
}

# Unexported utilities ----------------------------------------------
# These functions carry the the `_index` suffix for consistency with 
# the cell-valued sprinkles, but they don't actually require an 
# index, since they change table-valued sprinkles

sprinkle_html_preserve_index_assert <- function(html_preserve = getOption("pixie_html_preserve", TRUE),
                                                coll)
{
  checkmate::assert_logical(x = html_preserve,
                            len = 1,
                            add = coll,
                            .var.name = "html_preserve")
}

sprinkle_html_preserve_index <- function(x, html_preserve = getOption("pixie_html_preserve", TRUE), 
                                         indices = NULL, part = NULL)
{
  x[["html_preserve"]] <- html_preserve
  
  x
}
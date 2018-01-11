#' @name sprinkle_bookdown
#' @title Change the Bookdown Property in a Dust Table
#' 
#' @description Tables built for the \code{bookdown} package can be referenced
#'   in a manner that is consistent between HTML and LaTeX documents.
#'   
#' @param x An object of class \code{dust}
#' @param bookdown \code{logical(1)} indicating if the table is being produced
#'   in a \code{bookdown} document.
#' @param ... Additional arguments to pass to other methods. Currently ignored.
#'
#' @details \code{bookdown} is a package that facilitates the writing of books.
#'   One of the advantages of \code{bookdown} is the ability to reference 
#'   tables in a manner similar to LaTeX. The key difference in how 
#'   \code{pixiedust} handles output is the reference specification. See 
#'   \url{https://bookdown.org/yihui/bookdown/tables.html} for details on how
#'   \code{bookdown} uses labels and references.
#'   
#' @author Benjamin Nutter
#' 
#' @seealso \code{\link{dust}}, \code{\link{sprinkle}}
#' 
#' @source 
#' \url{https://bookdown.org/yihui/bookdown/tables.html}
#' 
#' @section Functional Requirements:
#' \enumerate{
#'  \item Change the \code{bookdown} attribute of the \code{dust} object.
#'  \item Cast an error if \code{x} is not a \code{dust} object.
#'  \item Cast an error if \code{bookdown} is not a logical object.
#'  \item Cast an error if \code{bookdown} has length greater than 1.
#' }
#' 
#' @export

sprinkle_bookdown <- function(x, 
                              bookdown = getOption("pixie_bookdown", FALSE), 
                              ...)
{
  UseMethod("sprinkle_bookdown")
}

#' @rdname sprinkle_bookdown
#' @export

sprinkle_bookdown.default <- function(x, 
                                      bookdown = getOption("pixie_bookdown", FALSE),
                                      ...)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = x,
                          classes = "dust",
                          add = coll)
  
  sprinkle_bookdown_index_assert(bookdown = bookdown,
                                 coll = coll)
  
  checkmate::reportAssertions(coll)
  
  sprinkle_bookdown_index(x = x,
                          bookdown = bookdown)
}

#' @rdname sprinkle_bookdown
#' @export

sprinkle_bookdown.dust_list <- function(x, 
                                        bookdown = getOption("pixie_bookdown", FALSE),
                                        ...)
{
  structure(
    lapply(x,
           sprinkle_bookdown.default,
           bookdown),
    class = "dust_list"
  )
}

# Unexported utilities ----------------------------------------------
# These functions carry the the `_index` suffix for consistency with 
# the cell-valued sprinkles, but they don't actually require an 
# index, since they change table-valued sprinkles

sprinkle_bookdown_index_assert <- function(bookdown = getOption("pixie_bookdown", FALSE), 
                                           coll)
{
  checkmate::assert_logical(x = bookdown,
                            len = 1,
                            add = coll,
                            .var.name = "bookdown")
}

# indices argument is only present to avoid errors when the argument is passed 
# from sprinkle
sprinkle_bookdown_index <- function(x, bookdown, indices = NULL)
{
  x[["bookdown"]] <- bookdown
  
  x
}
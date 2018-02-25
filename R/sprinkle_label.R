#' @name sprinkle_label
#' @title Change the Border Collapse Property in a Dust Table
#' 
#' @description The \code{label} property is used to make references to a 
#'   table.  Labels may be used in LaTeX documents, or in both LaTeX and 
#'   HTML documents when using \code{bookdown}.
#'   
#' @param x An object of class \code{dust}
#' @param label \code{character(1)} or \code{NULL} for no label.
#' @param ... Additional arguments to pass to other methods. Currently ignored.
#' 
#' @details For details about using labels in LaTeX documents, see 
#' \url{https://en.wikibooks.org/wiki/LaTeX/Labels_and_Cross-referencing}.
#' 
#' For details about using labels in \code{bookdown} documents, see
#' \url{https://bookdown.org/yihui/bookdown/tables.html}
#'   
#' @author Benjamin Nutter
#' 
#' @source \url{https://en.wikibooks.org/wiki/LaTeX/Labels_and_Cross-referencing}
#' 
#' \url{https://bookdown.org/yihui/bookdown/tables.html}
#' 
#' @seealso \code{\link{dust}}, \code{\link{sprinkle}}
#' 
#' @section Functional Requirements:
#' \enumerate{
#'  \item Change the \code{label} attribute of the \code{dust} object.
#'  \item Cast an error if \code{x} is not a \code{dust} object.
#'  \item Cast an error if \code{label} is not a \code{character(1)}.
#' }
#' 
#' @export

sprinkle_label <- function(x, label = NULL, ...)
{
  UseMethod("sprinkle_label")
}

#' @rdname sprinkle_label
#' @export

sprinkle_label.default <- function(x, label = NULL, ...)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = x,
                          classes = "dust",
                          add = coll)
  
  sprinkle_label_index_assert(label = label,
                              coll = coll)
  
  checkmate::reportAssertions(coll)
  
  sprinkle_label_index(x = x,
                       label = label)
}

#' @rdname sprinkle_label
#' @export

sprinkle_label.dust_list <- function(x, label = NULL, ...)
{
  warning("The same label will be applied to each table in this list. ",
          "This will likely result in problems with referncing tables. ",
          "Consider using `pixiemap` instead.")
  structure(
    lapply(x,
           sprinkle_label.default,
           label),
    class = "dust_list"
  )
}

# Unexported utilities ----------------------------------------------
# These functions carry the the `_index` suffix for consistency with 
# the cell-valued sprinkles, but they don't actually require an 
# index, since they change table-valued sprinkles

sprinkle_label_index_assert <- function(label = NULL, coll)
{
  checkmate::assert_character(x = label,
                              len = 1,
                              add = coll,
                              null.ok = TRUE,
                              .var.name = "label")
}

# indices argument is only present to avoid errors when the argument is passed 
# from sprinkle
sprinkle_label_index <- function(x, label, indices = NULL, part = NULL)
{
  x[["label"]] <- label
  
  x
}
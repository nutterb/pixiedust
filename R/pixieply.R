#' @name pixieply
#' @title Apply Functions Over `dust_list` Objects
#' 
#' @description The \code{sprinkle} methods work with \code{dust_list} 
#'   objects very naturally, but medleys pose a slightly more difficult problem.
#'   Medleys are intended to be predefined collections of sprinkles that reduce
#'   the time required to format a table with a particular look and style.  
#'   It seems counter-productive to expect a user to define each of her or his
#'   medleys as a method that can work with both \code{dust} and \code{dust_list}
#'   objects.  \code{pixieply} is a wrapper to \code{lapply} that preserves the
#'   \code{dust_list} class of the object.
#'   
#'   \code{pixiemap} provides functionality to apply differing sprinkles over
#'   each element of a \code{dust_list}.  The most common example is probably
#'   adding a unique caption to each table.
#'  
#' @param X An object of class \code{dust_list}.
#' @param FUN A function to apply to each element of \code{X}
#' @param ... Additional arguments to pass to \code{FUN}
#' @param MoreArgs a list of other arguments to FUN
#' @param SIMPLIFY logical or character string; attempt to reduce the result 
#'   to a vector, matrix or higher dimensional array; see the \code{simplify} 
#'   argument of \code{\link{sapply}}
#' @param USE.NAMES logical; use names if the first ... argument has names, 
#'   or if it is a character vector, use that character vector as the names.
#' 
#' @examples 
#' \dontrun{
#' #* This example will only display the last table 
#' #* in the viewer pane.  To see the full output,
#' #* run this example in an Rmarkdown document.
#' x <- split(mtcars, list(mtcars$am, mtcars$vs))
#' dust(x) %>%
#'   sprinkle_print_method("html") %>%
#'   pixieply(medley_bw)
#' }
#' 
#' \dontrun{
#' #* This is the full text of an RMarkdown script 
#' #* for the previous example.
#' ---
#' title: "Pixieply"
#' output: html_document
#' ---
#' 
#' ```{r}
#' library(pixiedust)
#' x <- dplyr::group_by(mtcars, am, vs)
#' dust(x, ungroup = FALSE) %>%
#'   sprinkle_print_method("html") %>%
#'     pixieply(medley_bw)
#' ```
#' }
#' 
#' @export

pixieply <- function(X, FUN, ...)
{
  checkmate::assertClass(X, "dust_list")
  
  structure(
    lapply(X = X,
           FUN = FUN,
           ...),
    class = "dust_list"
  )
}

#' @rdname pixieply
#' @export

pixiemap <- function(X, FUN, ..., MoreArgs = NULL, SIMPLIFY = FALSE, USE.NAMES = TRUE)
{
  checkmate::assertClass(X, "dust_list")
  
  structure(
    mapply(FUN = FUN, 
           X,
           ...,
           MoreArgs = MoreArgs,
           SIMPLIFY = SIMPLIFY,
           USE.NAMES = USE.NAMES),
    class = "dust_list"
  )
}
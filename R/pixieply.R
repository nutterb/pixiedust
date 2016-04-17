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
#' @param X An object of class \code{dust_list}.
#' @param FUN A function to apply to each element of \code{X}
#' @param ... Additional arguments to pass to \code{FUN}
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
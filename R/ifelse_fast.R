#' @name ifelse_fast
#' @export ifelse_fast
#' 
#' @title Faster ifelse
#' @description Perform an \code{ifelse} like function, but use subsetting instead of the 
#'   vectorized \code{ifelse}.  This has the potential to be much faster than \code{ifelse},
#'   which will be important when working on large tables.  It differs from \code{ifelse} in 
#'   that the vector on which replacements are being performed must already exist.
#'   
#' @param x A vector on which replacement should be performed.
#' @param test A logical expression
#' @param yes The values to impress upon \code{x} when \code{test} is \code{TRUE}.
#' @param no The values to impress upon \code{x} when \code{test} is \code{FALSE}.
#' 
#' @author Benjamin Nutter
#' 
#' @examples 
#' \dontrun{
#' set.seed(1)
#' x <- rep(NA, 1e7)
#'
#' notNA <- sample(1:1e7, 1e4)
#' x[notNA] <- "not missing"
#'
#' system.time({
#'   y1 <- ifelse(is.na(x), "", "font-weight:bold;")
#' })
#' 
#' system.time({
#'   y2 <- ifelse_fast(x, is.na(x), "", "font-weight:bold;")
#' })
#' }

ifelse_fast <- function(x, test, yes, no){
  logic <- eval(test)
  x[logic] <- yes
  x[!logic] <- no
  x
}
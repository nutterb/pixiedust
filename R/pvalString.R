#' @name pvalString
#' @importFrom lazyWeave pvalString
#' @export pvalString
#' 
#' @title Format P-values for Reports
#' @description Convert numeric p-values to character strings according to
#' pre-defined formatting parameters.  Additional formats may be added
#' for required or desired reporting standards.
#' 
#' @param p a numeric vector of p-values.
#' @param format A character string indicating the desired format for 
#'   the p-values.  See Details for full descriptions.
#' @param digits For \code{"exact"} and \code{"scientific"}; indicates the 
#'   number of digits to precede scientific notation.
#' @param ... Additional arguments to be passed to \code{format}
#' 
#' @details When \code{format = "default"}, p-values are formatted:
#' \enumerate{
#'   \item \emph{p > 0.99}: "> 0.99"
#'   \item \emph{0.99 > p > 0.10}: Rounded to two digits
#'   \item \emph{0.10 > p > 0.001}: Rounded to three digits
#'   \item \emph{0.001 > p}: "< 0.001"
#'  }
#'  
#'  When \code{format = "exact"}, the exact p-value is printed with the 
#'  number of significant digits equal to \code{digits}.  P-values smaller
#'  that 1*(10^-\code{digits}) are printed in scientific notation.
#'  
#'  When \code{format = "scientific"}, all values are printed in scientific
#'  notation with \code{digits} digits printed before the \code{e}.
#'  
#'  @author Benjamin Nutter
#'  @examples
#'  p <- c(1, .999, .905, .505, .205, .125, .09531,
#'         .05493, .04532, .011234, .0003431, .000000342)
#'  pvalString(p, format="default")
#'  pvalString(p, format="exact", digits=3)
#'  pvalString(p, format="exact", digits=2)
#'  pvalString(p, format="scientific", digits=3)
#'  pvalString(p, format="scientific", digits=4)
#'  

NULL
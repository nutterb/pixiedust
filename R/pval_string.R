#' @name pval_string
#' @export pval_string
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
#'  number of places after the deimal equal to \code{digits}.  P-values smaller
#'  that 1*(10^-\code{digits}) are printed in scientific notation.
#'  
#'  When \code{format = "scientific"}, all values are printed in scientific
#'  notation with \code{digits} digits printed before the \code{e}.
#'  
#' @section Functional Requirements:
#'  \enumerate{
#'   \item When \code{format = "default"}, print p-values greater than 
#'     0.99 as "> 0.99"; greater than 0.10 with two digits; 
#'     greater than 0.001 with three digits; and less than 0.001 as 
#'     "< 0.001".
#'   \item when \code{format = "exact"}, print the exact p-value out to at most
#'     \code{digits} places past the decimal place.
#'   \item When \code{format = "scientific"}, print the p-value in 
#'     scientific notation with up to \code{digits} values ahead of the 
#'     \code{e}.
#'   \item Cast an error if \code{p} is not numeric on the interval [0, 1]
#'   \item Cast an error if format is not one of \code{c("default", "exact",
#'     "scientific")}.
#'   \item Cast an error if \code{digits} is not \code{integerish(1)}.
#'  }
#'  
#' @author Benjamin Nutter
#'  
#' @examples
#'  p <- c(1, .999, .905, .505, .205, .125, .09531,
#'         .05493, .04532, .011234, .0003431, .000000342)
#'  pvalString(p, format="default")
#'  pvalString(p, format="exact", digits=3)
#'  pvalString(p, format="exact", digits=2)
#'  pvalString(p, format="scientific", digits=3)
#'  pvalString(p, format="scientific", digits=4)
#'  

pval_string <- function(p, format=c("default", "exact", "scientific"),
                       digits=3, ...){
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_numeric(x = p,
                            lower = 0,
                            upper = 1,
                            add = coll)
  
  format <- checkmate::matchArg(x = format,
                                choices = c("default", "exact", "scientific"),
                                add = coll)
  
  checkmate::assert_integerish(x = digits,
                               len = 1,
                               add = coll)
  
  checkmate::reportAssertions(coll)
  
  #* Alpha beta format
  if (format == "default"){
    ps <- ifelse(p > .99, 
                 "> 0.99",
                 ifelse(p > 0.10, 
                        format(round(p, 2), 
                               digits = 2),
                        ifelse(p >= 0.001, 
                               format(round(p, 3), 
                                      digits = 3), 
                               "< 0.001")))
  }
  
  #* exact format
  else if (format == "exact"){
    ps <- ifelse(p < 1*(10^-digits),
                 format(p, 
                        scientific = TRUE, 
                        digits=digits),
                 format(round(p, digits), 
                        digits = digits))
  }
  
  #* scientific notation format
  else if (format == "scientific"){
    ps <- format(p, 
                 scientific = TRUE, 
                 digits = digits) 
  }
  ps 
}

#' @rdname pval_string
#' @export

pvalString <- pval_string
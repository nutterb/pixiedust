#' @name assert_match_arg
#' @title Checkmate Compatible Version of \code{match.arg}
#' 
#' @description Matches arguments as \code{match.arg} and returns
#' the value of the selected argument.
#' 
#' @param x A character vecotr (of length one unless \code{several.ok} is \code{TRUE}
#'   or \code{NULL}.
#' @param choices A character vector of candidate values
#' @param several_ok logical specifying if \code{arg} should be allowed to have more 
#'   than one element.
#' @param ... Additional arguments to pass to either 
#'   \code{\link[checkmate]{assertChoice}} or \code{\link[checkmate]{assertSubset}}.
#'   \code{assertChoice} is used when \code{several_ok = FALSE}, otherwise 
#'   \code{assertSubset} is used.
#'   
#' @seealso \code{\link{match.arg}}, \code{\link[checkmate]{assertChoice}}, 
#'   \code{\link[checkmate]{assertSubset}}

assert_match_arg <- function(x, choices, several_ok = FALSE, ...)
{
  x <- choices[pmatch(x, choices, nomatch = 0L, duplicates.ok = several_ok)]
  if (several_ok) assertSubset(x, choices, ...)
  else assertChoice(x, choices, ...)
  x
}

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
#' @param .var.name \code{character(1)}, name of the checked object to print in 
#'   assertions.  Defaults to the heuristic implemented in 
#'   \code{\link[checkmate]{vname}}
#' @param add \code{AssertCollection}, Colleciton to store assertion messages.
#'   See \code{link[checkmate]{AssertCollection}}.
#' @param ... Additional arguments to pass to either 
#'   \code{\link[checkmate]{assertChoice}} or \code{\link[checkmate]{assertSubset}}.
#'   \code{assertChoice} is used when \code{several_ok = FALSE}, otherwise 
#'   \code{assertSubset} is used.
#'   
#' @seealso \code{\link{match.arg}}, \code{\link[checkmate]{assertChoice}}, 
#'   \code{\link[checkmate]{assertSubset}}

assert_match_arg <- function(x, choices, several_ok = FALSE, 
                             .var.name = checkmate::vname(x), add = NULL)
{
  if (identical(x, choices))
    if (several_ok) return(x) else return(x[1])
  
  if (!several_ok)
  {
    if (!checkmate::checkCharacter(x, len = 1))
    {
      add$push(sprintf("Assertions on '%s' failed: must have length 1, but has length %s",
                        .var.name, length(x)))
      #* If an AssertCollection is used, we want to mimic a failure
      #* to find a match.
      return(character(0)) 
    }
  }
  
  x <- choices[pmatch(x, choices, nomatch = 0L, duplicates.ok = several_ok)]
  if (several_ok) checkmate::assertSubset(x, choices, empty.ok = FALSE, 
                                          .var.name = .var.name, add = add)
  else checkmate::assertChoice(x, choices, 
                               .var.name = .var.name, add = add)
  #* If a match(es) is found, return it.  If no match is found
  #* and an AssertCollection is used, return character(0)
  x
}

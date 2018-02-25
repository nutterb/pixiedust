#' @name str_extract_base
#' @title Extract Patterns from Character Strings
#' 
#' @description This is a utility function that follow the pattern of
#'   \code{stringr::str_extract_all}.  It is provided to avoid the 
#'   dependency on the \code{stringr} package.
#'   
#' @param x \code{character} vector.
#' @param pattern \code{character(1)} of the pattern to find in \code{x}
#' @param n The number of splits.
#' 
#' @seealso \code{stringr::str_extract_all}
#' 
#' @source 
#' https://stackoverflow.com/a/27274231/1017276
#' 

str_extract_base <- function(x, pattern){
  do.call("rbind",
          regmatches(x, gregexpr(pattern, x)))
}

#' @rdname str_extract_base

str_split_fixed_base <- function(x, pattern, n){
  spl <- strsplit(x, pattern)
  
  spl <-
    lapply(spl,
           function(y) c(utils::head(y, n - 1),
                         paste0(utils::tail(y, -(n - 1)), collapse = " ")))
  
  do.call("rbind", spl)
}

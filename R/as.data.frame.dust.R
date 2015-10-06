#' @name as.data.frame.dust
#' 
#' @title Convert \code{dust} Object to Data Frame
#' @description Sprinkles are applied to the \code{dust} object
#'   as if it were being prepared for printing to the console.
#'   However, instead of printing, the object is returned 
#'   as a single data frame.
#'   
#' @param x A \code{dust} object.
#' @param ... Arguments to be passed to other methods.  Currently unused.
#' 
#' @details In its current state, this can be a fairly ineffcient function
#'   as the table, if the longtable option is in use, will be built in 
#'   a \code{for} loop and bound together using \code{rbind}.  This isn't 
#'   really intended for large tables, but may be of assistance when 
#'   there isn't a sprinkle that does what you want to do.  (You can 
#'   at least pull out the object as a data frame and do your own 
#'   post processing).
#'   
#' @author Benjamin Nutter
#' 
#' @examples 
#' fit <- lm(mpg ~ qsec + factor(am) + wt * factor(gear), data = mtcars)
#' Dust <- dust(fit) %>%
#'   sprinkle(cols = 2:4, round = 2) %>%
#'   sprinkle(cols = 5, fn = quote(pvalString(value))) %>%
#'   sprinkle(cols = 3, font_color = "#DA70D6") %>%
#'   sprinkle_print_method("html")
#'   
#' as.data.frame(Dust)
#' 
#' @export

as.data.frame.dust <- function(x, ...){
  print_dust_console(x, return_df = TRUE)
}


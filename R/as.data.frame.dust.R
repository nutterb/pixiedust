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
#' @param sprinkled Logical.  If \code{TRUE}, the sprinkles attached to the
#'   \code{dust} object are applied before returning the data frame. 
#'   Sprinkles are applied via the same mechanism that prints to the console,
#'   so only sprinkles that are applicable to console output are used.
#'   When \code{FALSE}, \code{pixiedust} attempts to reconstruct the 
#'   data frame (or tidied output from \code{broom::tidy} 
#'   originally given to \code{dust}.
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

as.data.frame.dust <- function(x, ..., sprinkled = TRUE){
  if (sprinkled){
    return(print_dust_console(x, return_df = TRUE))
  }
  else {
    X <- dplyr::select(x$body, 
                       row, col, value) %>%
      tidyr::spread(col, value) %>%
      dplyr::select(-row)
    
    col_names <- dplyr::group_by(x$body, col) %>%
      dplyr::summarise(col_name = col_name[1])
    col_names <- col_names$col_name
    
    classes <- dplyr::group_by(x$body, col) %>%
      dplyr::summarise(col_class = col_class[1])
    classes <- paste0("as.", classes$col_class)
    
    for (i in seq_along(X)){
      X[[i]] <- get(classes[i])(X[[i]])
    }
    
    names(X) <- col_names
    
    X
  }
  
}

utils::globalVariables(c("value", "col_name", "col_class"))

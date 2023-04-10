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
#' @details In its current state, this can be a fairly inefficient function
#'   as the table, if the longtable option is in use, will be built in 
#'   a \code{for} loop and bound together using \code{rbind}.  This isn't 
#'   really intended for large tables, but may be of assistance when 
#'   there isn't a sprinkle that does what you want to do.  (You can 
#'   at least pull out the object as a data frame and do your own 
#'   post processing).
#'   
#' @author Benjamin Nutter
#' 
#' @section Functional Requirements: 
#' \enumerate{
#'  \item Accepts an object of class \code{dust} or \code{dust_list}
#'  \item Accepts a \code{logical(1)} indicating if the sprinkles should
#'    be applied to the data.
#'  \item For a \code{dust} object, returns an object of class 
#'    \code{data.frame}
#'  \item For a \code{dust_list} object, returns a list of objects of class
#'    \code{data.frame}
#' }
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

as.data.frame.dust <- function(x, ..., sprinkled = TRUE)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = x,
                          classes = "dust",
                          add = coll)
  
  checkmate::assert_logical(x = sprinkled,
                            len = 1,
                            add = coll)
  
  checkmate::reportAssertions(coll)
  
  
  if (sprinkled)
  {
    return(print_dust_console(x, return_df = TRUE))
  }
  else 
  {
    X <- x$body[c("row", "col", "value")]
    X <- reshape2::dcast(X, 
                         formula = row ~ col, 
                         value.var = "value")
    X <- X[!names(X) %in% "row"]

    col_names <- tapply(X = x$body$col_name, 
                        INDEX = x$body$col, 
                        FUN = function(x) x[1])
    
    col_names <- unname(col_names)
    names(X) <- col_names
    
    classes <- tapply(X = x$body$col_class, 
                      INDEX = x$body$col, 
                      FUN = function(x) x[1])
    classes <- unname(classes)
    classes <- sprintf("as.%s", classes)
    
    for (i in seq_along(X)){
      X[[i]] <- get(classes[i])(X[[i]])
    }
    
    X
  }
  
}

#' @rdname as.data.frame.dust
#' @export

as.data.frame.dust_list <- function(x, ...)
{
  checkmate::assert_class(x = x,
                          classes = "dust_list")
  
  lapply(x,
         as.data.frame.dust,
         ...)
}

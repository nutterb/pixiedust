#' @name rbind_internal
#' @title Bind Rows in Base R
#' 
#' @description Stack data frames on top of each other. Data frames do not have to 
#'   have all of the same columns.
#'   
#' @param ... data frames
#' @param deparse.level See \code{deparse.level} in \code{rbind}.
#' 
#' @author Benjamin Nutter

.rbind_internal <- function(..., deparse.level = 1){
  df_list <- list(...)
  
  df_list <- df_list[!vapply(df_list, is.null, logical(1))]
  
  all_data_frame <- vapply(X = df_list, 
                           FUN = inherits, 
                           FUN.VALUE = logical(1), 
                           what = "data.frame")
  
  if (!all(all_data_frame)){
    stop("All objects in ... must be data frames")
  }
  
  all_name <- unique(unlist(lapply(df_list, names)))
  
  df_list <-
    lapply(df_list, 
           function(d){
             miss_var <- setdiff(all_name, names(d))
             miss_frame <- lapply(miss_var, 
                                  function(x) rep(NA, nrow(d)))
             miss_frame <- as.data.frame(miss_frame, 
                                         stringsAsFactors = FALSE)
             names(miss_frame) <- miss_var
             if (nrow(miss_frame) > 0){
               cbind(d, miss_frame)
             } else {
               d
             }
             
           })
  
  do.call("rbind", df_list)
}

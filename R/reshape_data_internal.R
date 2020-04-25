#' @name reshape_data_internal
#' 
#' @title Reshape data frames for Pixiedust
#' @description Pixiedust reshapes data to have one row per cell in the table. This 
#'   permits adjustments to be made to individual cells.  These internal functions
#'   are provided to simplify the reshaping process. It is slower than using 
#'   the tidyr functions `gather` and `spread` (or whatever their newer counterparts 
#'   are), but keeps me off of other people's development schedules.
#'   
#' @param data A \code{data.frame}
#' 
#' @details No validations are performed in these functions, and it is assumed that
#'   the input data set has the components it needs.
#'   
#' @author Benjamin Nutter
#'

.make_dataframe_long <- function(data){
  out <- stats::reshape(data = data, 
                        varying = list(names(data)),
                        direction = "long")
  
  names(out) <- c("col", "value", "row")
  
  out$col_name <- names(data)[match(out$col, seq_along(names(data)))]
  
  out <- out[c("row", "col", "col_name", "value")]
  
  rownames(out) <- NULL
  
  out
}

.make_dataframe_wide <- function(data){
  col_order <- unique(data$col_name)
  
  out <- reshape2::dcast(data, 
                         row ~ col_name, 
                         value.var = "value")
  
  out[col_order]
}
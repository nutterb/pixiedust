#* perform_function
#* An internal function for dustpan
#* applies the requested function.  
#* Applying the function wasn't really straight forward because
#* in dustpan$obj, all of the values are stored as 
#* character strings.  Handling the conversions has to
#* be done with care to get things to format correctly

perform_function <- function(obj)
{
  #* Determine which cells in the table have a function assigned.
  have_fn <- which(!is.na(obj$fn))
  
  for (i in have_fn){
    #* All of the elements in 'value' are stored as character 
    #* strings. The if clause allows numeric functions to be
    #* performed.
    if (obj$col_class[i] %in% c("double", "numeric", "integer"))
      value <- do.call(sprintf("as.%s", obj$col_class[i]), 
                       list(obj$value[i]))
    #* The else statement allows functions to act on character strings.
    else value <- obj$value[i]
      
    res <- eval(parse(text = obj$fn[i]))
    
    obj$value[i] <- res
    obj$col_class[i] <- primaryClass(res)
  }
  
  obj
}

#*** roundSafe
#* An internal function to perform rounding on dust objects.
#* All values in a dust object are stored as character values, but some may
#* represent numeric values.  The roundSafe function will skip true character
#* values when attempting to round.

roundSafe <- function(x, digits){
  y <- suppressWarnings(as.numeric(x))
  if (length(y[!is.na(y)]) == 0) return(x)
  
  y[!is.na(y)] <- round(y[!is.na(y)], digits[!is.na(y)])
  x[!is.na(y)] <- y[!is.na(y)]
  x
}
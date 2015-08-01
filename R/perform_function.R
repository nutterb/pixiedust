#* perform_function
#* An internal function for dustpan
#* applies the requested function.  
#* Applying the function wasn't really straight forward because
#* in dustpan$obj, all of the values are stored as 
#* character strings.  Handling the conversions has to
#* be done with care to get things to format correctly

perform_function <- function(obj)
{
  have_fn <- which(!is.na(obj$fn))
  
  for (i in have_fn){
    if (obj$col_class[i] %in% c("double", "numeric", "integer"))
      value <- do.call(paste0("as.", obj$col_class[i]), 
                       list(obj$value[i]))
    else value <- obj$value[i]
      obj$value[i] <- eval(parse(text = obj$fn[i]))
  }
  
  obj
}
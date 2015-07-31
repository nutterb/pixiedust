.onLoad <- function(libname,pkgname)
{
  options(pixiedust_print_method="console")
}

.onUnload <- function(libPath)
{
  options(pixiedust_print_method=NULL)
}
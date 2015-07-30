.onLoad <- function(libname,pkgname)
{
  options(fairydust_print_method="console")
}

.onUnload <- function(libPath)
{
  options(dustpan_output=NULL)
}
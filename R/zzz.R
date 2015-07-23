.onLoad <- function(libname,pkgname)
{
  options(dustpan_output="console")
}

.onUnload <- function(libPath)
{
  options(dustpan_output=NULL)
}
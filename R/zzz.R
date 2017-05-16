.onLoad <- function(libname,pkgname)
{
  options(pixie_count = 0L)
}

.onUnload <- function(libPath)
{
  options(pixie_count = NULL)
}

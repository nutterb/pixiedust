.onLoad <- function(libname,pkgname)
{
  options(pixiedust_print_method="console",
          pixie_count = 0)
}

.onUnload <- function(libPath)
{
  options(pixiedust_print_method=NULL,
          pixie_count = NULL)
}
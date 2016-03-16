.onLoad <- function(libname,pkgname)
{
  options(pixiedust_print_method = "console",
          pixiedust_latex_hhline = FALSE,
          pixie_count = 0)
}

.onUnload <- function(libPath)
{
  options(pixiedust_print_method=NULL,
          pixiedust_latex_hhline = NULL,
          pixie_count = NULL)
}
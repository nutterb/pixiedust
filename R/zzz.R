.onAttach <- function(libname,pkgname)
{
  packageStartupMessage("Additional documentation is being constructed at ",
                        "http://nutterb.github.io/pixiedust/index.html")
}

.onLoad <- function(libname,pkgname)
{
  options(pixiedust_print_method = "console",
          pixie_count = 0L)
}

.onUnload <- function(libPath)
{
  options(pixiedust_print_method=NULL,
          pixie_count = NULL)
}
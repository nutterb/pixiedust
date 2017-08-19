.onAttach <- function(libname,pkgname)
{
  packageStartupMessage("Additional documentation is being constructed at ",
                        "http://nutterb.github.io/pixiedust/index.html")
}

.onLoad <- function(libname,pkgname)
{
  options(pixie_count = 0L)
}

.onUnload <- function(libPath)
{
  options(pixie_count = NULL)
}

## Test environments
* local Linux install (R-3.6.1; #97-Ubuntu SMP Wed Apr 1 03:25:46 UTC 2020)
* remote Linux install (4.0.0; Ubuntu 16.04.6 LTS, Travis CI)
* win-builder (release R )
* win-builder-devel ()

## R CMD check results
This update removes some dependencies and will avoid errors that would arise due to a future release of the `dplyr` package.

There were no warnings, errors, or notes returned by CHECK on any of the 
test environments.


## Downstream dependencies
Reverse dependency checks showed the following:

`tadaatoolbox` shows no warnings or errors. There is a note regarding found UTF-8 strings. This note exists in the CRAN checks online as well.

`HydeNet` shows no warnings, errors, or notes.

Thanks,
Benjamin

## Test environments
* local Windows install (R-4.0.3; Windows 10 build 18363)
* remote Linux install (4.0.2; Ubuntu 16.04.6 LTS, Travis CI)
* win-builder (release R 4.0.3; 2020-10-10)
* win-builder-devel (2021-01-13 r79826)

## R CMD check results
This update to `pixiedust` was requested by CRAN to address the use of `order` on a data frame.

There were no warnings, errors, or notes returned by CHECK on any of the 
test environments.


## Downstream dependencies
Reverse dependency checks showed the following:

`tadaatoolbox` shows no warnings or errors. There is a note regarding found UTF-8 strings. This note exists in the CRAN checks online as well.

`wiseR` shows no warnings or errors. There is a note for 'Namespaces in Imports field not imported from'. This note exists in the CRAN checks as well.

`HydeNet` shows 2 errors that appear to be related to deprecated functions in the `dplyr` package.  There is also a NOTE about a package listed in the imports that is not imported from. These errors do not appear to be caused by changes in `pixiedust`. If it is acceptable to you, I would like to proceed with this update to `pixiedust` prior to fixing problems in `HydeNet`.

Thanks,
Benjamin

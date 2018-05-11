## Test environments
* local Linux install (R-3.4.3; #135-Ubuntu SMP Fri Jan 19 11:48:36 UTC 2018)
* remote Linux install (R-3.4.2; ubuntu 4.8.4-2ubuntu1~14.04.3)
* win-builder (release R 3.5.0)
* win-builder (2018-05-05 r74699)

## R CMD check results
This update corrects one of the tests related to a change in how errors are reported from the `checkmate` package.


## Downstream dependencies
`HydeNet` and `tadaatoolbox` show no warnings, errors, or notes.

Thanks,
Benjamin

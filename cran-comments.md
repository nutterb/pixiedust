## Test environments
* local Linux install (R-3.5.2; #166-Ubuntu SMP Wed Nov 14 20:09:47 UTC 2018)
* remote Linux install (R-3.5.1; Ubuntu 14.04.5 LTS, Travis CI)
* win-builder (release R 3.5.2)
* win-builder-devel (2019-01-09 r75961)

## R CMD check results
This update correst method registration issues reported to me by Kurt Hornik on 4 December 2018.   

This also addresses impacts that will happen due to an upcoming release of dplyr.

There were no warnings, errors, or notes returned by CHECK on any of the 
test environments.


## Downstream dependencies
`tadaatoolbox` show no warnings, errors, or notes.

`HydeNet` produces a NOTE that it cannot find the required package `graph`. This seems to be related to an improper dependency in a dataset that was reported to me by Professor Ripley.  I have submitted a concurrent update to CRAN for the `HydeNet` package to remove this dependency.

Thanks,
Benjamin

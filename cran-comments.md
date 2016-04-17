## Test environments
* local Windows install (R-3.2.4)
* x86_64-pc-linux-gnu (64-bit) (on travis-ci), R 3.2.4
* win-builder (devel and release)

## R CMD check results
There were no ERRORS or WARNINGS in any of the checks.

NOTE: on win-builder (devel and release)
The note flags potentially misspelled words in the package
description.  All words are correctly spelled.


## Downstream dependencies
`HydeNet` fails checks due to errors in the unit tests.  This is related
to updates in the `testthat` package.  I am the maintainer of `HydeNet` 
and am planning to submit an update to `HydeNet` after `dplyr` is updated
later this month.

`tadaatoolbox` checks return the NOTE "Note: found 1 marked UTF-8 string",
which is also found in its check results on CRAN.  `tadaatoolbox` otherwise
passes all checks.

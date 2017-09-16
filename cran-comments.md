## Test environments
* local Windows install (R-3.4.1)
* x86_64-pc-linux-gnu (Ubuntu precise 12.04.5 64-bit) (on travis-ci), R 3.4.1
* win-builder (devel 2017-09-12 r73242)

## R CMD check results
At the request of a user, this update reduces the dependency on R to 3.1.2.  It also fixes a handful of bugs identified while developing additional tests.

There were no ERRORS or WARNINGS in any of the checks.

NOTE: on win-builder 
The note flags potentially misspelled words in the package
description.  All words are correctly spelled.


## Downstream dependencies
`HydeNet` shows no warnings, errors, or notes.

I was unable to complete the checks on `pointblank`; my system could not 
resolve the Java dependencies.  I have notified the maintainer that I am 
submitting this update to CRAN, as he is also trying to lower the R version dependency.

`tadaatoolbox` generates a warning about an undocumented argument. I have 
notified the maintainer; their fix has been applied but not yet submitted to CRAN

Thanks,
Benjamin

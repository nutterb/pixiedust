## Test environments
* local Windows install (R-3.4.1)
* local Windows install (R-devel 2017-08-24 r73125)
* x86_64-pc-linux-gnu (Ubuntu precise 12.04.5 64-bit) (on travis-ci), R 3.4.1
* win-builder (devel 2017-08-25 r73125)

## R CMD check results
There were no ERRORS or WARNINGS in any of the checks.

NOTE: on win-builder 
The note flags potentially misspelled words in the package
description.  All words are correctly spelled.


## Downstream dependencies
`HydeNet` shows no warnings, errors, or notes.

I was unable to complete the checks on `pointblank`; my system could not 
resolve the Java dependencies.  I have notified the maintainer that I am 
submitting this update to CRAN.  I have also reviewed the `pointblank`
source code and am confident that the changes in this submission will not 
produce any new errors or warnings for `pointblank`.

`tadaatoolbox` generates a warning about an undocumented argument. I have 
notified the maintainer.

Thanks,
Benjamin

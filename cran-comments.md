## Test environments
* local Linux install (R-3.4.3; #135-Ubuntu SMP Fri Jan 19 11:48:36 UTC 2018)
* x86_64-pc-linux-gnu (Ubuntu trusty 14.04.5 64-bit) (on travis-ci), R 3.4.2
* win-builder (release R 3.4.3)
* win-builder (devel 2017-09-12 r73242)

## R CMD check results



## Downstream dependencies
`HydeNet` and `pointblank` show no warnings, errors, or notes.

I was unable to complete the checks on `pointblank`; my system could not 
resolve the Java dependencies.  I have notified the maintainer that I am 
submitting this update to CRAN. I will note that the current version of 
`pointblank` on CRAN shows errors. These are unrelated to `pointblank`, and
the development version on GitHub no longer uses `pixiedust` as a dependency.  
I have notified the `pointblank` maintainer of this submission.


Thanks,
Benjamin

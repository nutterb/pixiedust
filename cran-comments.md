## Test environments
* local Linux install (R-3.4.3; #135-Ubuntu SMP Fri Jan 19 11:48:36 UTC 2018)
* remot Linux install (R-3.4.2; ubuntu 4.8.4-2ubuntu1~14.04.3)
* win-builder (release R 3.4.3)
* win-builder (devel 2018-02-21 r74285)

## R CMD check results
There is a note about a change in the Maintainer e-mail address. I am unable 
to confirm the change from nutter@battelle.org as the e-mail address doesn't
exist.  I have sent an e-mail from CRAN from my personal e-mail requesting 
that the maintainer e-mail address be changed to benjamin.nutter@gmail.com.


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

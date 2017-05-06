## Test environments
* local Windows install (R-3.4.0 and devel 3.5.0 2015-05-04 r72654)
* x86_64-pc-linux-gnu (64-bit) (on travis-ci), R 3.2.4
* win-builder (devel 3.5.0 and release 3.4.0)

## R CMD check results
Resubmission note: package names are now single-quoted in description field.

There were no ERRORS or WARNINGS in any of the checks.

NOTE: on win-builder (devel and release)
The note flags potentially misspelled words in the package
description.  All words are correctly spelled.


## Downstream dependencies
`HydeNet`: The changes in this version of `pixiedust`
prevent installation errors that would be introduced by an upcoming version
of `dplyr`. `HydeNet` passes all checks under the current and upcoming version
of `dplyr`.

`tadaatoolbox` passes all checks.

Thanks,
Benjamin

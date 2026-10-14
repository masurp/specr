## Submission

This is a minor release (1.0.0 -> 1.1.0). It adds the function `infer_curve()` for inference on the specification curve, fixes several bugs, and updates the code for current versions of ggplot2, dplyr, tidyr, and purrr (e.g., it no longer uses the deprecated `purrr::cross()`). See NEWS.md for details.

## Test environments

* local: macOS (Apple silicon), R 4.6.1
* GitHub Actions: macOS (release), Windows (release), Ubuntu (devel, release, oldrel-1)
* win-builder: R-devel
* mac-builder: R-release

## R CMD check results

0 errors | 0 warnings | 1 note

* The note concerns the URL https://urisohn.com/specification-curve/ (in README.md), which returns status 406 to the automated URL check, but is accessible in a web browser.

## Reverse dependencies

There are no reverse dependencies on CRAN.

## pkgcheck version 0.3.4

The previous submission was failing, although it is no longer. I nevertheless received a resubmit-or-archive email, and so am submitting this updated version.

Note that the previous failures were test failures from external calls which did not "fail gracefully." I have made every effort to ensure this package is as fully testable on CRAN as possible. I firmly believe that this submission now fails gracefully on all attempts at external calls within the entire test suite.

## R CMD check results

This submission generates no ERRORs or WARNINGs on the platforms listed below. It does generate notes on some systems that "github.com" URLs are unavailable, but these are automated rejections of definitively valid URLs.

GitHub actions:
* Linux: R-release, R-devel, R-oldrelease
* OSX: R-release
* Windows: R-release, R-devel, R-oldrelease

CRAN win-builder:
* R-oldrelease, R-release, R-devel

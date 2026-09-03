## pkgcheck version 0.3.0

This is a new submission. Note that many tests have `testthat::skip_on_cran()` flags, because the package performs many checks specific to GitHub, the platform used for rOpenSci's software review. These require an API token. As many tests as possible are nevertheless run on CRAN machines, including thorough checks of the general input/output structures of this package, and all main workflow stages.

## R CMD check results

This submission generates no ERRORs or WARNINGs on the platforms listed below.

The package has been checked on all environments listed below, and generates only the single note identifying the package as a new submission.

GitHub actions:
* Linux: R-release, R-devel, R-oldrelease
* OSX: R-release
* Windows: R-release, R-devel, R-oldrelease

CRAN win-builder:
* R-oldrelease, R-release, R-devel

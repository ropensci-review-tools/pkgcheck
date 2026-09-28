## pkgcheck version 0.3.1

This submission fixes all failing tests flagged in email from 28th Sep 2026. All of these tests now entirely avoid external API calls, so will always "fail gracefully" on all test systems.

## R CMD check results

This submission generates no ERRORs or WARNINGs on the platforms listed below. It does generate notes on some systems that "github.com" URLs are unavailable, but these are automated rejections of definitively valid URLs.

GitHub actions:
* Linux: R-release, R-devel, R-oldrelease
* OSX: R-release
* Windows: R-release, R-devel, R-oldrelease

CRAN win-builder:
* R-oldrelease, R-release, R-devel

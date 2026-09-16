## pkgcheck version 0.3.0

This is a new submission. Note that many tests have `testthat::skip_on_cran()` flags, because the package performs many checks specific to GitHub, the platform used for rOpenSci's software review. These require an API token. As many tests as possible are nevertheless run on CRAN machines, including thorough checks of the general input/output structures of this package, and all main workflow stages.

This submission fixes issues identified in previous initial submission attempt, including:

- The DOI entry in DESCRIPTION has been fixed as instructed.
- Stopped one vignette writing to non-tempdir location.
- I think warnings about unexecutable code have been fixed, although I was unable to reproduce those.
- Almost all `dontrun{}` fences converted to `donttest{}`, except:
  - In `pkgcheck_bg()` which launches a background process which will run for an undefined length of time.
  - In `use_github_action_pkgcheck()` which downloads remote files.
  - Five other functions which run the main `pkgcheck()` function on an example R package and take > 5s on most machines tested.

## R CMD check results

This submission generates no ERRORs or WARNINGs on the platforms listed below.
It does generate notes on some systems that "github.com" URLs are unavailable, but these are automated rejections of definitively valid URLs.

The package has been checked on all environments listed below, and generates only the single note identifying the package as a new submission.

GitHub actions:
* Linux: R-release, R-devel, R-oldrelease
* OSX: R-release
* Windows: R-release, R-devel, R-oldrelease

CRAN win-builder:
* R-oldrelease, R-release, R-devel

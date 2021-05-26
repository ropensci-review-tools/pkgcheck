# pkgcheck

<!-- badges: start -->

[![R build
status](https://github.com/ropenscilabs/pkgcheck/workflows/R-CMD-check/badge.svg)](https://github.com/ropenscilabs/pkgcheck/actions?query=workflow%3AR-CMD-check)
[![gitlab
push](https://github.com/ropenscilabs/pkgcheck/workflows/push-to-gitlab/badge.svg)](https://github.com/ropenscilabs/pkgcheck/actions?query=workflow%3Apush-to-gitlab)
[![Project Status:
Concept](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

Check whether a package is ready for submission to
[rOpenSci](https://ropensci.org)’s peer review system. The primary
function collates the output of
[`goodpractice`](https://github.com/mangothecat/goodpractice), including
`R CMD check` results, along with a number of statistics via the
[`pkgstats` package](https://github.com/ropenscilabs/pkgstats), and
package structure checks expected for rOpenSci submissions. The output
of this function immediately indicates whether or not a package is
“Ready to Submit”.

Installation of the [`pkgstats`
package](https://github.com/ropenscilabs/pkgstats) also requires both
[`ctags`](https://ctags.io) and [GNU
`global`](https://www.gnu.org/software/global/) to be installed. See
package description and those links for how to install those libraries
on your system. This package also uses the [github GraphQL
API](https://developer.github.com/v4) which requires a local GitHub
token to be stored with an unambiguous name including `GITHUB`, such as
`GITHUB_TOKEN` (recommended), or `GITHUB_PAT` (for Personal
Authorization Token). This can be obtained from GitHub (via your user
settings), and stored using

``` r
Sys.setenv("GITHUB_TOKEN" = "<my_token>")
```

This can also be set permanently by putting this line in your
`~/.Renviron` file (or creating this if it does not yet exist). Once
`pkgstats` has been successfully installed, the `pkgcheck` package can
then be loaded via a `library` call:

``` r
library(pkgcheck)
```

## Usage

The package primarily has one function, `pkgcheck`, which accepts the
single argument, `path`, specifying the local location of a git
repository to be analysed. The following code generates a reproducible
report by first downloading a local clone of a repository called
[`srr-demo`](https://github.com/mpadge/srr-demo), which contains the
skeleton of an [`srr` (Software Review Roclets)
package](https://github.com/ropenscilabs/srr), generated with the
[`srr_stats_pkg_skeleton()`
function](https://ropenscilabs.github.io/srr/reference/srr_stats_pkg_skeleton.html):

``` r
library (gert)
mydir <- file.path (tempdir (), "srr-demo")
git_clone ("https://github.com/mpadge/srr-demo", path = mydir)
x <- pkgcheck (mydir)
```

That object has default `print` and `summary` methods. The latter can be
used to simply check whether a package is ready for submission:

``` r
summary (x)
## 
## ── demo 0.0.0.9000 ────────────────────────────────────────────────────────────────────────────────────────────────────
## 
## ✔ Package uses 'roxygen2'
## ✖ Package does not have a 'contributing.md' file
## ✖ Package does not have a 'CITATION' file
## ✖ Package does not have a 'codemeta.json' file
## ✔ All functions have examples
## ✔ Package  Package 'DESCRIPTION' has a URL field
## ✖ Package  Package 'DESCRIPTION' does not have a BugReports field
## ✔ Package name is available
## ✖ Package has no continuous integration checks
## ✖ Package coverage is 0% (should be at least 75%)
## ✔ R CMD check found no errors
## ✔ R CMD check found no warnings
## ✔ All applicable standards have been documented in this package
## 
## ℹ Current status:
## ✖ This package is not ready to be submitted
## 
```

A package may only be submitted when the summary contains all ticks and
no cross symbols. (These symbols are colour-coded with green ticks and
red crosses when generated in a terminal; GitHub markdown only renders
them in black-and-white.) The object returned from the `pkgcheck`
function is a complex nested list with around a dozen primary
components. Full information can be obtained by simply calling the
default `print` method by typing the object name (`x`).

## What is checked?

Calling `summary()` on the object returned by the [`pkgcheck()`
function](https://ropenscilabs.github.io/pkgcheck/reference/pkgcheck.html)
will generate a checklist like that shown above. This checklist will
also be automatically generated when a package is first submitted to
rOpenSci, and is used by the editors to assess whether to process a
submission. Authors must ensure prior to submission that there are no
red crosses in the resultant list. (In the unlikely circumstances that a
package is unable to pass particular checks, explanations should be
given upon submission about why those checks fail, and why review may
proceed in spite of such failures.)

The full list of checks which packages are expected to pass currently
includes:

1.  Package must use [`roxygen2`](https://roxygen2.r-lib.org) for
    documentation.
2.  Package must have a [`contributing.md`
    file](https://devguide.ropensci.org/collaboration.html#contributing-guide).
3.  Package must have a [`CITATION` file in the `inst`
    directory](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#CITATION-files).
4.  Package must have a [`codemeta.json`
    file](https://devguide.ropensci.org/building.html#creating-metadata-for-your-package).
5.  All exported functions must include examples in their documentation.
6.  Left-assign operators must be used consistently throughout all code
    (so either all `=` or all `<-`, but not a mixture of both).
7.  Package `DESCRIPTION` file must have a “URL” field.
8.  Package `DESCRIPTION` file must have a “BugReports” field.
9.  Package name must be available (or package must already be) on CRAN.
10. Package must have continuous integration tests.
11. Package must have test coverage of at least 75%.
12. `R CMD check` must generate no warnings or errors.
13. All statistical standards must be documented, as confirmed by the
    [`srr::srr_pre_submit()`
    function](https://ropenscilabs.github.io/srr/reference/srr_stats_pre_submit.html).

## Caching and running `pkgcheck` in the background

Running the [`pgkcheck`
function](https://ropenscilabs.github.io/pkgcheck/reference/pkgcheck.html)
can be time-consuming, primarily because the
[`goodpractice`](https://github.com/mangothecat/goodpractice) component
runs both a full `R CMD check`, and calculates code coverage of all
tests. To avoid re-generating these results each time, the package saves
previous reports to a local cache, in a `pkgcheck` subdirectory of the
location determined by

``` r
rappdirs::user_cache_dir()
```

You may manually erase the contents of this subdirectory at any time at
no risk beyond additional time required to re-generate contents. This
default location may also be over-ridden by setting an environmental
variable named `pkgcheck_cache_dir`. By default checks presume packages
use `git` for version control, with checks updated only when code is
updated via `git commit`. Checks for packages that do not use `git` are
updated when any files are modified.

The first time
[`pkgcheck()`](https://ropenscilabs.github.io/pkgcheck/reference/pkgcheck.html)
is applied to a package, the checks will be stored in the cache
directory. Calling that function a second time will then load the cached
results, and so enable checks to be returned much faster. For code which
is frequently updated, such as for packages working on the final stages
prior to submission, it may still be necessary to repeatedly call
[`pkgcheck()`](https://ropenscilabs.github.io/pkgcheck/reference/pkgcheck.html)
after each modification, a step which may still be inconveniently
time-consuming. To facilitate frequent re-checking, the package also has
a [`pkgcheck_bg()`
function](https://ropenscilabs.github.io/pkgcheck/reference/pkgcheck_bg.html)
which is effectively identical to the main [`pkgcheck()`
function](https://ropenscilabs.github.io/pkgcheck/reference/pkgcheck.html),
except it runs in the background, enabling you to continue coding while
checks are running.

The [`pkgcheck_bg()`
function](https://ropenscilabs.github.io/pkgcheck/reference/pkgcheck_bg.html)
returns a handle to the [`callr::r_bg()`
process](https://callr.r-lib.org/reference/r_bg.html) in which the
checks are running. Typing the name of the returned object will
immediately indicate whether the checks are still running, or whether
they have finished. That handle is itself an [`R6`
object](http://r6.r-lib.org/) with a number of methods, notably
including
[`get_result()`](https://callr.r-lib.org/reference/get_result.html)
which can be used to access the checks once the process has finished.
Alternatively, as soon as the background process, the normal
(foreground) [`pkgcheck()`
function](https://ropenscilabs.github.io/pkgcheck/reference/pkgcheck.html)
may be called to quickly re-load the cached results.

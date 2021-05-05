# pkgreport

<!-- badges: start -->

[![R build
status](https://github.com/ropenscilabs/pkgreport/workflows/R-CMD-check/badge.svg)](https://github.com/ropenscilabs/pkgreport/actions?query=workflow%3AR-CMD-check)
[![gitlab
push](https://github.com/ropenscilabs/pkgreport/workflows/push-to-gitlab/badge.svg)](https://github.com/ropenscilabs/pkgreport/actions?query=workflow%3Apush-to-gitlab)
[![Project Status:
Concept](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

Check whether a package is ready for submission to
[rOpenSci](https://ropensci.org)’s peer review system. The primary
function collates a number of statistics via the
[`pkgstats`](https://github.com/ropenscilabs/pkgstats) package which is
not on CRAN and must first be installed with

``` r
remotes::install_github("ropenscilabs/pkgstats")
```

That package also requires both [`ctags`](https://ctags.io) and [GNU
`global`](https://www.gnu.org/software/global/) to be installed. See
package description and those links for how to install those libraries
on your system.

Once `pkgstats` has been successfully installed, the `pkgreport` package
can then be loaded via a `library` call:

``` r
library(pkgreport)
```

This package also uses the [github GraphQL
API](https://developer.github.com/v4) which requires a local github
token to be stored with an unambiguous name including `GITHUB` and maybe
`QL`, if alternative `GITHIB` tokens already exist. This can be obtained
from github (via your user settings), and stored using

``` r
Sys.setenv("GITHUB_QL" = "<my_token>")
```

This can also be set permanently by putting this line in your
`~/.Renviron` file (or creating this if it does not yet exist).

The package also works by locally caching reports previously analysed
packages, in a `pkgreport` subdirectory of the location determined by

``` r
rappdirs::user_cache_dir()
```

You may manually erase the contents of this subdirectory at any time at
no risk. The location may also be over-ridden by setting an
environmental variable named `pkgreport_cache_dir`.

## Usage

The package primarily has one function, `pkgreport`, which accepts the
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
x <- pkgreport (mydir)
```

That object has default `print` and `summary` methods. The latter can be
used to simply check whether a package is ready for submission:

``` r
summary (x)
```

    ## 

    ## ── demo 0.0.0.9000 ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────

    ## 

    ## 

    ## ── Package Summary ──

    ## 

    ## ✔ Package uses 'roxygen2'

    ## ✖ Package does not have a 'contributing.md' file

    ## ✔ All exported functions have examples

    ## ✔ Package 'DESCRIPTION' has a URL field

    ## ✖ Package 'DESCRIPTION' has no BugReports field

    ## ✖ Package does not have continuous integration checks

    ## ✖ Package coverage is 0% (should be at least 75%)

    ## ✔ R CMD check found no errors

    ## ✔ R CMD check found no warnings

    ## ✔ All applicable standards have been documented in this package

    ## 

    ## ℹ Current status:

    ## ✖ This package is not ready to be submitted

    ## 

A package may only be submitted when the summary contains all ticks and
no cross symbols. (And these symbols are colour-coded when generated in
a terminal; GitHub markdown only renders them in black-and-white.) The
full details of the object returned from the `pkgreport` function may be
seen with the default `print` method:

``` r
print (x)
```

    ## 

    ## ── demo 0.0.0.9000 ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────

    ## 

    ## 

    ## ── Package Summary ──

    ## 

    ## ✔ Package uses 'roxygen2'

    ## ✖ Package does not have a 'contributing.md' file

    ## ✔ All exported functions have examples

    ## ✔ Package 'DESCRIPTION' has a URL field

    ## ✖ Package 'DESCRIPTION' has no BugReports field

    ## ✖ Package does not have continuous integration checks

    ## ✖ Package coverage is 0% (should be at least 75%)

    ## ✔ R CMD check found no errors

    ## ✔ R CMD check found no warnings

    ## ✔ All applicable standards have been documented in this package

    ## 

    ## ℹ Current status:

    ## ✖ This package is not ready to be submitted

    ## 

    ## 

    ## ── git ──

    ## 

    ## • HEAD: 77dfe392

    ## • Default branch: main

    ## • Number of commits: 5

    ## • First commit: 22-02-2021

    ## • Number of authors: 1

    ## 

    ## 

    ## ── Statistical Standards ──

    ## 

    ## ✔ All applicable standards have been documented in this package

    ## ℹ Category

    ## • Regression and Supervised Learning

    ## ℹ 'srr' report is at [/home/smexus/.cache/pkgreport/static/srr-demo_srr77dfe392.html]

    ## 

    ## 

    ## ── Package Structure ──

    ## 

    ## ℹ Package uses the following languages:

    ## • R: 31%

    ## • C++: 69%

    ## 

    ## ℹ Package has

    ## • 1 author

    ## • 0 vignettes

    ## • No internal data

    ## • 1 imported package

    ## • No exported functions

    ## • 4 non-exported functions (median 3 lines of code)

    ## • 3 C++ functions (median 4 lines of code)

    ## • 0 parameters per function

    ## 

    ## ── All statistics

    ##                 measure value percentile noteworthy
    ## 1               files_R   4.0       23.3           
    ## 2             files_src   2.0       77.4           
    ## 4       files_vignettes   0.0        0.0       TRUE
    ## 5           files_tests   2.0       64.1           
    ## 6                 loc_R  10.0        0.4       TRUE
    ## 7               loc_src  22.0        0.3       TRUE
    ## 8             loc_tests   6.0        4.2       TRUE
    ## 9         num_vignettes   0.0        0.0       TRUE
    ## 12              n_fns_r   4.0        0.5       TRUE
    ## 13     n_fns_r_exported   0.0        0.0       TRUE
    ## 14 n_fns_r_not_exported   4.0        2.6       TRUE
    ## 15            n_fns_src   3.0       77.0           
    ## 16     n_fns_per_file_r   1.0        0.0       TRUE
    ## 17   n_fns_per_file_src   1.5        6.7           
    ## 19    num_params_per_fn   0.0        0.0       TRUE
    ## 21         loc_per_fn_r   3.0        2.3       TRUE
    ## 23 loc_per_fn_r_not_exp   3.0        4.0       TRUE
    ## 25       loc_per_fn_src   4.0        1.8       TRUE
    ## 26 fn_call_network_size   1.0        0.3       TRUE

    ## 

    ## ℹ Package network diagram is at [/home/smexus/.cache/pkgreport/static/srr-demo_pkgstats77dfe392.html]

    ## 

    ## 

    ## ── goodpractice ──

    ## 

    ## ── GP demo ─────────────────────────────────────────────────────────────────────
    ## 
    ## It is good practice to
    ## 
    ##   ✖ write unit tests for all functions, and all package code in
    ##     general. 0% of code lines are covered by test cases.
    ## 
    ##     R/test.R:11:NA
    ##     src/cpptest.cpp:9:NA
    ##     src/cpptest.cpp:10:NA
    ## 
    ##   ✖ add a "BugReports" field to DESCRIPTION, and point it to a bug
    ##     tracker. Many online code hosting services provide bug trackers for
    ##     free, https://github.com, https://gitlab.com, etc.
    ##   ✖ avoid long code lines, it is bad for readability. Also, many people
    ##     prefer editor windows that are about 80 characters wide. Try make
    ##     your lines shorter than 80 characters
    ## 
    ##     R/srr-stats-standards.R:12:1
    ##     R/srr-stats-standards.R:13:1
    ##     R/srr-stats-standards.R:14:1
    ##     R/srr-stats-standards.R:15:1
    ##     R/srr-stats-standards.R:16:1
    ##     ... and 96 more lines
    ## 
    ## ────────────────────────────────────────────────────────────────────────────────

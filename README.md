# pkgreport

<!-- badges: start -->

[![R build
status](https://github.com/ropenscilabs/pkgreport/workflows/R-CMD-check/badge.svg)](https://github.com/ropenscilabs/pkgreport/actions?query=workflow%3AR-CMD-check)
[![gitlab
push](https://github.com/ropenscilabs/pkgreport/workflows/push-to-gitlab/badge.svg)](https://github.com/ropenscilabs/pkgreport/actions?query=workflow%3Apush-to-gitlab)
[![Project Status:
Concept](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

Plumber API to report on package structure and function. Uses
functionality provided by the
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

The package also works by locally caching previously analysed packages,
in a `pkgreport` subdirectory of the location determined by

``` r
rappdirs::user_cache_dir()
```

You may manually erase the contents of this subdirectory at any time at
no risk.

## Usage

The server associated with this package can be built by cloning this
repository, and modifying the associated
[`Dockerfile`](https://github.com/ropenscilabs/pkgreport/blob/master/Dockerfile)
by inserting a GitHub token (as `GITHUB_PAT`). Then in the local
directory holding a clone of this repo:

    docker build -t pkgreport .
    docker run -it -p 8000:8000 --rm pkgreport

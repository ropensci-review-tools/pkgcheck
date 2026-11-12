# Contributing to pkgcheck

## Opening issues

The easiest way to note any behavioural curiosities or to request any new
features is by opening a [github
issue](https://github.com/ropensci-review-tools/pkgcheck/issues).


## Development guidelines

If you'd like to contribute changes to `pkgcheck`, we use [the GitHub
flow](https://docs.github.com/en/get-started/quickstart/github-flow) for proposing,
submitting, reviewing, and accepting changes. If you haven't done this before,
there's a nice [overview of git](https://r-pkgs.org/git.html), as well
as [best practices for submitting pull requests](http://r-pkgs.org/git.html#pr-make)
in the R packages book by Hadley Wickham and Jenny Bryan.

The `pkgcheck` coding style diverges somewhat from [the commonly used tidyverse style
guide](https://style.tidyverse.org/syntax.html#spacing), primarily through judicious use of
whitespace, which aims to improve code readability. Code references in
`pkgcheck` are separated by whitespace, just like words of text. Just like it
is easier to understand "these three words" than "thesethreewords", code is not
formatted like this:

``` r
these <- three(words(x))
```
rather like this:

``` r
these <- three (words (x))
```

The position of brackets is then arbitrary, and we could also write

``` r
these <- three( words (x))
```

`pkgcheck` code opts for the former style, with the natural result that one
ends up writing

```r
this <- function ()
```

with a space between `function` and `()`. That's it.

## Adding new checks

New checks are a welcome contribution to `pkgcheck`, for which there is a
[dedicated
vignette](https://docs.ropensci.org/pkgcheck/articles/extending-checks.html).
Please discuss any proposed new checks by opening an issue on the GitHub
repository.


## Notes for maintainers: tokens and secrets

### `RRT_TOKEN`

"RRT" stands for "rOpenSci Review Tools". `RRT_TOKEN` is a personal access
token stored as a secret in this repository. The [Docker
workflow](https://github.com/ropensci-review-tools/pkgcheck/blob/main/.github/workflows/docker.yaml)
uses it, after building and pushing the `pkgcheck` Docker image, to trigger the
`publish.yaml` workflow of
[`pkgcheck-action`](https://github.com/ropensci-review-tools/pkgcheck-action).
The default `GITHUB_TOKEN` can't do that, because it only has access to the
repository the workflow runs in.

The token must be generated from the personal GitHub settings of someone with
admin rights on both repositories. Give it a short expiry (60 days at most).
When it expires, the "Trigger pkgcheck-action build" step of the Docker
workflow fails. To renew it, generate a new token and paste it into the
`RRT_TOKEN` secret of this repository. A copy of the token is also stored as a
secret in `pkgcheck-action`, but none of that repository's workflows currently
use it.

### Other secrets

The Docker image is pushed to the GitHub Container Registry
(`ghcr.io/ropensci-review-tools/pkgcheck`) with the default `GITHUB_TOKEN`, so
it needs no other secret. The image is the base of [the `roreviewapi`
Dockerfile](https://github.com/ropensci-review-tools/roreviewapi/blob/main/Dockerfile),
so any change to where the image is published must also be made there.

`CODECOV_TOKEN` is used by the test coverage workflow. The other secrets in this
repository (`DOCKER_USERNAME`, `DOCKER_PASSWORD`, `EMAIL`, `GH_TOKEN`, `GLPW`,
`NETLIFY_AUTH_TOKEN`, `NETLIFY_SITE_ID` and `UNAME`) are not used by any current
workflow.


## Code of Conduct

We want to encourage a warm, welcoming, and safe environment for contributing to
this project. See the [code of
conduct](https://ropensci.org/code-of-conduct/) for
more information.

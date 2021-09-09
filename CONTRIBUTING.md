# Contributing to pkgcheck

## Opening issues

The easiest way to note any behavioural curiosities or to request any new
features is by opening a [github
issue](https://github.com/ropensci-review-tools/pkgcheck/issues).


## Development guidelines

If you'd like to contribute changes to `pkgcheck`, we use [the GitHub
flow](https://guides.github.com/introduction/flow/index.html) for proposing,
submitting, reviewing, and accepting changes. If you haven't done this before,
there's a nice overview of git [here](http://r-pkgs.had.co.nz/git.html), as well
as best practices for submitting pull requests
[here](http://r-pkgs.had.co.nz/git.html#pr-make).

The `pkgcheck` coding style diverges somewhat from [this commonly used R style
guide](http://adv-r.had.co.nz/Style.html), primarily through judicious use of
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


## Code of Conduct

We want to encourage a warm, welcoming, and safe environment for contributing to
this project. See the [code of
conduct](https://github.com/ropensci-review-tools/pkgcheck/blob/master/CODE_OF_CONDUCT.md) for
more information.

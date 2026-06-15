
-- pkgstats 9.9 ----------------------------------------------------------------

v Package is already on CRAN.
x does not have a 'contributing' file.
x The following functions have no documented return values: [ctags_install, desc_stats, rd_stats, tags_data]
v uses 'roxygen2'.
v 'DESCRIPTION' has a URL field.
v 'DESCRIPTION' has a BugReports field.
x Package has no HTML vignettes
x These functions do not have examples: [pkgstats_from_archive].
v Repository has a website
v Package has continuous integration checks, but no badges on README
v All goodpractice linters passed.
x Package contains unexpected files.
x Default GitHub branch of 'master' is not acceptable.
v This is a statistical package which complies with all applicable standards
x All examples use `\dontrun`.
i Package depends on the following obsolete packages: [blah,sp]

i Current status:
x Sorry about that, but your package is not quite ready ...

i 'pkgcheck' version: 7.8.9


-- git --

* HEAD:
* Default branch:
* Number of commits:
* First commit:
* Number of authors:


-- rOpenSci Statistical Standards --

i Compliance with rOpenSci statistical standards:
v srr message
i 'srr' report is at [].


-- Package Structure --

i Package uses the following languages:
* C++: 9%
* R: 91%

i Package has
* 1 author.
* 0 vignettes.
* No internal data
* 9 imported packages.
* 11 exported functions (median 43 lines of code).
* 53 non-exported functions (median 14 lines of code).
* 12 C++ functions (median 16 lines of code).
* 1 parameters per function (median).

-- Package statistics --


i Package network diagram is not here.


-- goodpractice --


x always use `add = TRUE` in `on.exit()` calls. Without it, each `on.exit()`
  overwrites previous exit handlers, which is a common source of bugs.

    'R/tag-data.R:153'

x keep functions short and focused. Long functions are harder to understand,
  test, and maintain. Consider splitting into smaller helpers. (Default limit
  of 50 set with option 'goodpractice.function_length_limit').

    'R/archive-trawl.R:37'
    'R/ctags-test.R:16'
    'R/desc-stats.R:33'
    'R/external-calls.R:49'
    'R/external-calls.R:204'
    ... and 13 more lines

x remove or use internal functions that are defined but never called. Dead code
  increases maintenance burden.

    'R/ctags-test.R:142'
    'R/ctags-test.R:150'
    'R/onload.R:2'

x add a 'README.md' (or 'README.Rmd') file to the top-level directory. A good
  README describes what the package does, how to install it, and includes a
  short example.


x add a 'NEWS.md' file to track user-visible changes between releases. See
  <https://style.tidyverse.org/news.html> for formatting guidance.


x Add examples to all exported functions.

    'man/pkgstats_from_archive.Rd'

x Document return values for exported (non-method) functions using `\\value`.

    'man/ctags_install.Rd'
    'man/desc_stats.Rd'
    'man/rd_stats.Rd'
    'man/tags_data.Rd'

x define exported (user-facing) functions before internal helper functions
  within each R source file.

    'R/ctags-install.R:5'
    'R/ctags-install.R:31'
    'R/ctags-install.R:42'
    'R/desc-stats.R:10'
    'R/loc.R:12'
    ... and 1 more line


-- Other checks --

x Package contains the following unexpected files:
  * a
  * b
x Package contains the following (potentially) obsolete packages:
  * blah
  * sp
  * rgdal

See our [Recommended
Scaffolding](https://devguide.ropensci.org/building.html?q=scaffol#recommended-scaffolding)
for alternatives.

-- Package Versions --

  pkgstats: 42
  pkgcheck: 42

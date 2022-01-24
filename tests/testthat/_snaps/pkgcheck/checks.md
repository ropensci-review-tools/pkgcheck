## Checks for [testpkgchecknotapkg (v0.0.0.9000)]()

git hash: [](/tree/)

- :heavy_check_mark: Package name is available
- :heavy_multiplication_x: does not have a 'CITATION' file.
- :heavy_multiplication_x: does not have a 'codemeta.json' file.
- :heavy_multiplication_x: does not have a 'contributing' file.
- :heavy_check_mark: uses 'roxygen2'.
- :heavy_multiplication_x: 'DESCRIPTION' does not have a URL field.
- :heavy_multiplication_x: 'DESCRIPTION' does not have a BugReports field.
- :heavy_multiplication_x: Package has no HTML vignettes
- :heavy_multiplication_x: These functions do not have examples: [test_fn].
- :heavy_multiplication_x: Continuous integration checks unavailable (no URL in 'DESCRIPTION').
- :heavy_multiplication_x: Statistical standards are missing
- :heavy_multiplication_x: This package still has TODO standards and can not be submitted

**Important:** All failing checks above must be addressed prior to proceeding

Package License: GPL-3

---

### 1. rOpenSci Statistical Standards ([`srr` package](https://github.com/ropensci-review-tools/srr))

This package is in the following category:

- *Regression and Supervised Learning*

:heavy_multiplication_x: This package still has TODO standards and can not be submitted

Click to see the [report of author-reported standards compliance of the package with links to associated lines of code](report.html), which can be re-generated locally by running the [`srr_report()` function](https://docs.ropensci.org/srr/reference/srr_report.html) from within a local clone of the repository.

---


### 2. Statistical Properties

This package features some noteworthy statistical properties which may need to be clarified by a handling editor prior to progressing.

<details>
<summary>Details of statistical properties (click to open)</summary>
<p>

The package has:

- code in C++ (72% in 2 files) and R (28% in 4 files)
- 1 authors
- no  vignette
- no internal data file
- 1 imported package
- 1 exported function (median 3 lines of code)
- 2 non-exported functions in R (median 3 lines of code)
- 2 R functions (median 5 lines of code)

---

Statistical properties of package structure as distributional percentiles in relation to all current CRAN packages
The following terminology is used:
- `loc` = "Lines of Code"
- `fn` = "function"
- `exp`/`not_exp` = exported / not exported

The final measure (`fn_call_network_size`) is the total number of calls between functions (in R), or more abstract relationships between code objects in other languages. Values are flagged as "noteworthy" when they lie in the upper or lower 5th percentile.

|measure                 | value| percentile|noteworthy |
|:-----------------------|-----:|----------:|:----------|
|files_R                 |     4|       28.3|           |
|files_src               |     2|       79.1|           |
|files_vignettes         |     0|        0.0|TRUE       |
|files_tests             |     2|       68.6|           |
|loc_R                   |    10|        0.8|TRUE       |
|loc_src                 |    26|        4.0|TRUE       |
|loc_tests               |     6|        4.7|TRUE       |
|num_vignettes           |     0|        0.0|TRUE       |
|n_fns_r                 |     3|        2.5|TRUE       |
|n_fns_r_exported        |     1|        0.0|TRUE       |
|n_fns_r_not_exported    |     2|        2.7|TRUE       |
|n_fns_src               |     2|        4.3|TRUE       |
|n_fns_per_file_r        |     1|        0.2|TRUE       |
|n_fns_per_file_src      |     1|        0.1|TRUE       |
|num_params_per_fn       |     0|        0.0|TRUE       |
|loc_per_fn_r            |     3|        1.1|TRUE       |
|loc_per_fn_r_exp        |     3|        1.5|TRUE       |
|loc_per_fn_r_not_exp    |     3|        1.5|TRUE       |
|loc_per_fn_src          |     5|        5.0|TRUE       |
|rel_whitespace_R        |    40|        4.3|TRUE       |
|rel_whitespace_src      |    27|        5.8|           |
|rel_whitespace_tests    |    17|        2.4|TRUE       |
|doclines_per_fn_exp     |     6|        0.8|TRUE       |
|doclines_per_fn_not_exp |     0|        0.0|TRUE       |
|fn_call_network_size    |     1|       11.4|           |

---

</p></details>


### 2a. Network visualisation

An interactive visualisation of calls between objects in the package has been uploaded as a workflow artefact. To view it, click on results from the [latest 'pkgcheck' action](network.html), scroll to the bottom, and click on the 'visual-network' artefact.

---

### 3. `goodpractice` and other checks

('goodpractice' not included with these checks)

---

<details>
<summary>Package Versions</summary>
<p>

|package  |version   |
|:--------|:---------|
|pkgstats |42    |
|pkgcheck |42    |
|srr      |42    |

</p>
</details>

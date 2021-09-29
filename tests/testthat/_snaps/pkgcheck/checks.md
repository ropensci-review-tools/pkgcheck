## Checks for [testpkgchecknotapkg (v0.0.0.9000)]()

git hash: [](/tree/)

- :heavy_check_mark: Package uses 'roxygen2'.
- :heavy_multiplication_x: Package does not have a 'contributing.md' file.
- :heavy_multiplication_x: Package does not have a 'CITATION' file.
- :heavy_multiplication_x: Package does not have a 'codemeta.json' file.
- :heavy_multiplication_x: These functions do not have examples: [test_fn.Rd].
- :heavy_multiplication_x: Package has at no HTML vignettes
- :heavy_multiplication_x: Package 'DESCRIPTION' does not have a URL field.
- :heavy_multiplication_x: Package 'DESCRIPTION' does not have a BugReports field.
- :heavy_check_mark: Package name is available.
- :heavy_multiplication_x: Continuous integration checks unavailable (no URL in 'DESCRIPTION').
- :heavy_multiplication_x: Package coverage is 0% (should be at least 75%).
- :heavy_check_mark: This package still has TODO standards and can not be submitted.

**Important:** All failing checks above must be addressed prior to proceeding

Package License: GPL-3

---

## 1. rOpenSci Statistical Standards ([`srr` package](https://github.com/ropensci-review-tools/srr))

This package is in the following category:

- *Regression and Supervised Learning*

:heavy_check_mark: This package still has TODO standards and can not be submitted

Click [here to see the report of author-reported standards compliance of the package with links to associated lines of code](report.html), which can be re-generated locally by running the [`srr_report()` function](https://docs.ropensci.org/srr/reference/srr_report.html) from within a local clone of the repository.

---


## 2. Statistical Properties

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
|files_R                 |     4|       23.3|           |
|files_src               |     2|       77.4|           |
|files_vignettes         |     0|        0.0|TRUE       |
|files_tests             |     2|       64.1|           |
|loc_R                   |    10|        0.4|TRUE       |
|loc_src                 |    26|        0.3|TRUE       |
|loc_tests               |     6|        4.2|TRUE       |
|num_vignettes           |     0|        0.0|TRUE       |
|n_fns_r                 |     3|        0.4|TRUE       |
|n_fns_r_exported        |     1|        1.0|TRUE       |
|n_fns_r_not_exported    |     2|        0.3|TRUE       |
|n_fns_src               |     2|       76.3|           |
|n_fns_per_file_r        |     1|        0.0|TRUE       |
|n_fns_per_file_src      |     1|        0.0|TRUE       |
|num_params_per_fn       |     0|        0.0|TRUE       |
|loc_per_fn_r            |     3|        2.3|TRUE       |
|loc_per_fn_r_exp        |     3|        1.4|TRUE       |
|loc_per_fn_r_not_exp    |     3|        4.0|TRUE       |
|loc_per_fn_src          |     5|        5.5|           |
|rel_whitespace_R        |    40|        2.9|TRUE       |
|rel_whitespace_src      |    27|       76.6|           |
|rel_whitespace_tests    |    17|       61.0|           |
|doclines_per_fn_exp     |     6|        0.7|TRUE       |
|doclines_per_fn_not_exp |     0|        0.0|TRUE       |
|fn_call_network_size    |     1|        0.3|TRUE       |

---

</p></details>


### 2a. Network visualisation

Interactive network visualisation of calls between objects in package can be viewed by [clicking here](network.html)

---

## 3. `goodpractice` and other checks

<details>
<summary>Details of goodpractice and other checks (click to open)</summary>
<p>


---



</p>
</details>

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

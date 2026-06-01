# pkgcheck News

## 0.1.2

### New checks and check improvements

- Added check to make sure statistical standards document compliance for
  at least one specific category, not just general.
- Added check for packages using statistical standards only in a single
  directory or file (`check-srr`), improving detection of incomplete standards
  adoption.
- Elevated `sp` to the "truly obsolete" category in the obsolete-package-deps
  check.
- Added `rm_global_assign_in_memoise` to handle `<<-` within `memoise` calls,
  preventing false positives in the global-assignment check.
- Fixed output of `obsolete_pkg_deps` function and improved ordering of checks
  via `order_checks`.

### Infrastructure and CI

- Added `dependabot.yml` to keep GitHub Actions dependencies up to date;
  bumped `actions/checkout`, `actions/github-script`,
  `docker/build-push-action`, `docker/setup-buildx-action`, and
  `docker/login-action` to current versions.
- Updated check-standard workflow; improved test-coverage workflow.
- Switched off `bspm` before final `sf` reinstall in Dockerfile to fix
  library-linkage issues; re-installed `sf` at end of Docker script.
- Added all packages from `r-universe-org/base-image` to Dockerfile.
- Updated test snapshots for new `srr` standards (v0.2) and for changes in
  `pkgstats` output.

### Bug fixes

- Fixed minor bug in `parse_pkg_deps` and `order_checks`.
- Fixed `catch lintr errors` so that lintr failures no longer crash the full
  check run.
- Improved `render_markdown`/`render_md2html` and added examples and return
  values to all exported functions.

### Documentation

- Updated README to describe the package-check workflow more clearly.
- Updated monthly GHA workflow message.
- Renamed `render_markdown` to `render_md2html` for clarity.
- Added `pkgcheck` GitHub Action integration (`add pkgcheck action`).

---

## 0.1.1

### New checks

- Added `check-fns-have-return-vals`: flags exported functions whose
  roxygen documentation lacks a `@return` tag.
- Added `check-default-branch`: warns when the default git branch is not
  `main`.
- Implemented a three-way symbol set (`heavy_check_mark`, `eyes`,
  `heavy_multiplication_x`) to distinguish clear passes, marginal results, and
  clear failures across all check output.

### Check improvements

- Rewrote the srr failure message to list each missing standard individually,
  making it actionable for package authors.
- Improved `fns_have_return_vals` to use `tools::parse_Rd` instead of
  `roxygen2` for parsing return values, making it more robust.
- Removed `fns_have_return_vals` from the `print` method; results now appear
  only in the `summary` method.
- Fixed print output when an srr check fails entirely.

### Documentation and vignettes

- Restructured README with a clearer check-by-check guide.
- Added `list-checks` vignette documenting every check.
- Updated all `pkgchk_` function documentation pages; added `roxygen2 family`
  tags throughout.
- Added `@schneiderpy` to the contributors list.

### Infrastructure

- Switched Docker base image from `bspm` to `r2u` for faster, more reliable
  package installation.
- Switched Dockerfile base OS from `ubuntugis` to `ubuntugis-unstable` to fix
  spatial library compatibility.
- Added missing `roxygen2 family` tag for the `check-has-codemeta` family.
- Moved seasonal packages from the `roreviewapi` Dockerfile to this one.
- Updated `allcontributors` list.

---

## 0.1.0

### New checks

- Added `check-unique-fn-names`: verifies that no exported function name
  duplicates a name already used by a CRAN package (`fn_names_on_cran`).
- Added `check-renv` / `renv_activated`: detects packages where `renv` is
  activated in the project, which can interfere with dependency resolution.

### Check improvements

- Renamed `has_renv` check to `renv_activated` with an improved user-facing
  message.
- Changed `misc_check_counts` to a dedicated summary function replacing
  `has_misc_checks`.
- Improved `format-checks.R` to handle checks with empty print methods.
- Fixed `check_left_assigns` to correctly handle edge cases.
- Fixed `pkgchk_fns_have_exs` to exclude `package` and `data` docTypes from
  the examples check.
- Added `rm_global_assigns_in_ref_class` to avoid false positives from
  `<<-` inside R5/Reference Class definitions.

### API and caching

- Moved the cache directory from `cache/` to `cache/R/` for cleaner
  organisation.
- Added `use_cache` parameter to the main `pkgcheck` function, allowing callers
  to bypass the cache.
- Renamed `cache_pkgstats_component` to `cache_pkgcheck_...` for naming
  consistency.
- Switched CI-results fetching from `httr` / `jsonlite` to `jsonlite` only.

### Infrastructure and CI

- Added `goodpractice` as a GitHub remote dependency (temporarily removed from
  CRAN) in all workflows and Dockerfile.
- Moved `Julia` pre-installation to the Dockerfile; added explicit branch
  parameter to `get_latest_commit`.
- Updated `roxygen2` to 7.2.1; updated various workflow job names.
- Added `use_github_action_pkgcheck` documentation; described `pkgcheck-action`
  in README.
- Renamed the check workflow job from `check` to `pkgcheck`.

### Documentation

- Described testing new checks in the "Extending Checks" vignette.

---

## 0.0.3

### New checks

- Added `check-license`: validates that the package `LICENSE` field is a
  recognised CRAN-compatible identifier (closes #73).
- Added `check-pkgdown`: detects whether a `pkgdown` site exists and is
  configured (closes #100).
- Added `check-obsolete-pkg-deps`: flags dependencies on packages considered
  truly or potentially obsolete on CRAN (closes #44).
- Added GitHub-Actions default-branch check: warns when the default branch
  name differs from `main`.

### Check improvements

- Improved `check-fns-have-exs` to exclude dataset documentation entries
  (closes #103).
- Improved `check-on-cran` with a `tryCatch` around the CRAN URL test.
- Improved `check-scrap` to detect the same scrap file appearing in different
  locations.
- Deactivated the `roxygen2 family` tag check (closes #100) as it produced
  many false positives.
- Fixed `pkgchk_uses_roxygen2`; fixed `check_fns_have_exs` logic.

### Markdown and print output

- Implemented `msg_pre` / `msg_post` print sub-methods for all check types,
  providing a consistent before/after hook pattern (closes #108).
- Added `pkgfns_as_details` section to markdown output, listing all exported
  function names in a collapsible block (closes #117, #137).
- Added `pkgdeps_as_table` section to markdown output, listing package
  dependencies and their imported call counts.
- Added Bootstrap-style alerts (`alert-success`, `alert-danger`) in HTML
  output (closes #82).
- Added tooltip descriptions to stats summary output (closes #28).
- Implemented markdown rendering of the "extra checks" section (closes #44,
  #108).
- Fixed `checks_to_markdown` scrap-check key name.

### Dependency data

- Added `get_re_exports` and `ncalls` column to the package-dependency summary.
- Added `external_fns` data item to the checks object, exposing counts of
  calls to functions in each imported package.

### CI and GitHub Actions

- Added `use_github_action_pkgcheck` / `use_github_check` function for
  inserting the pkgcheck GHA template into a package (closes #135).
- Added `branch` parameter to `use_github_action_pkgcheck` so the action
  targets the correct branch (closes #135).
- Added `NOT_CRAN` environment variable to the GHA template (fixes #134).
- Added `read_pkg_guide` helper for reading the rOpenSci packaging guide.
- Removed GitLab mirroring workflow (closes #128).
- Added `monthly.yaml` workflow to send automated monthly check summaries
  (closes #123).
- Added `allcontributors` integration.
- Switched `visjs` path handling to be compatible with GHA runners (closes
  #127).
- Added note in rendered output when network diagram is generated on GHA
  (closes #127).

### Testing and snapshots

- Added `clean-snapshots.R` script to normalise platform-specific paths in
  snapshot output (closes #111).
- All tests now use a local `tempdir()` as the cache directory, isolating them
  from the real user cache (closes #113).
- Added `test-extra-checks` and associated snapshots.
- Added `test-order-checks.R`.
- Switched to `pandoc --wrap=preserve` for reproducible HTML snapshot output.
- Added `goodpractice = FALSE` parameter to the main `pkgcheck` function,
  allowing tests (and users) to skip the slow goodpractice step.

### Infrastructure

- Replaced `ghql` dependency with `gh` for all GitHub API calls (closes #256
  prep).
- Replaced `httr` with `httr2` for all HTTP requests.
- Removed `cloc` dependency.
- Updated Dockerfile: R 4.0 → 4.1, added `ubuntugis` PPA, pre-installed
  spatial system libraries, pre-installed `arrow`, `duckdb`, `sf`.
- Added `add-apt-repository` / `software-properties-common` to Docker image.
- Added spaceout pre-commit hook; applied consistent whitespace formatting
  across all source files.
- Updated `rappdirs`-based cache path to use `fs::path_abs`.
- Added `.onUnload` to reset options and environment variables set during
  package load.

---

## 0.0.2

### Package rename and restructure

- Renamed the package from an earlier prototype to **pkgcheck** and updated
  the object class, all exported function names, and documentation to match
  (closes #21).
- Restructured the main `pkgcheck()` object: separated `info` items
  (git metadata, pkgstats, srr, CI badges, URLs) from `checks` items (each
  individually named `pkgchk_*`).
- Split the monolithic `format-checks.R` into `collate-checks.R` and
  individual per-check source files under `R/check-*.R` and `R/info-*.R`.
- Renamed all `collate_*` functions to `summarise_*` for consistency.
- Added a generic `summarise_check` function dispatched per check type.

### New checks

- `check-has-citation`: verifies a `CITATION` file exists (closes #9).
- `check-has-codemeta`: verifies a `codemeta.json` file exists (closes #9).
- `check-has-contrib`: verifies a `CONTRIBUTING.md` file exists (closes #9,
  #35).
- `check-has-vignette`: detects presence of at least one built vignette
  (improved by Maëlle Salmon).
- `check-scrap`: detects unwanted files such as `.DS_Store` in the git tree
  (closes #27).
- `check-pkgname-available`: checks that the package name is not already on
  CRAN.
- `check-left-assign`: reports use of `<-` vs `=` for assignment in function
  bodies.
- `check-fns-have-exs`: reports exported functions lacking `@examples` tags.
- `check-on-cran`: reports whether the package is already on CRAN.
- `check-uses-roxygen2`: checks that `roxygen2` is used for documentation.
- `check-covr`: reports test coverage via `covr`.

### Output and print methods

- Added `print` and `summary` S3 methods for the `pkgcheck` class; added
  dedicated `output_pkgchk_*` functions for each check type, providing
  consistent screen and markdown rendering.
- Added `checks_to_markdown` function that converts the full `pkgcheck` result
  to a GitHub-flavoured Markdown string, including a visjs network diagram and
  an srr standards section.
- Added `render_markdown` function to render the Markdown string to HTML via
  `rmarkdown`.
- Implemented `symbol_tck` / `symbol_crs` helpers (tick and cross Unicode
  symbols) used uniformly across all output functions; added `fmt` parameter
  for screen vs. markdown formatting.
- Added `order_checks` function to control the display sequence of check items
  in summary output.
- Added `list_pkgchecks` function (with `quiet` parameter) to enumerate all
  currently active checks.
- Added `extra_check_prints_from_env` allowing extra checks supplied via an
  environment variable to appear in the print output (closes #40).

### goodpractice integration

- Separated `goodpractice` into its own `pkgchk_gp` check; divided gp results
  into four labelled sections (rcmdcheck, cyclocomp, lintr, covr); added
  `lintr` section to output (closes #19).
- Added `get_gp_report` (exported) and local caching of gp results in the
  cache directory.
- Extended `convert_gp_components` to produce Markdown-formatted sub-sections.

### Statistical software standards (srr)

- Integrated the `srr` package: captured the srr version, copied the srr
  Markdown report to the static cache directory, and added srr categories to
  the print and summary output.
- Added `add_srr_categories_from_report` function.
- Added separate `collate_srr_checks` / `summarise_srr_checks` functions.

### Caching and background execution

- Added `pkgcheck_bg` for running checks in a background `callr` process
  (via `callr::r_bg`), with `checks_running_in_bg` to detect concurrent runs.
- Added `stdout_stderr_cache` / `logfile_names` helpers for routing
  background-process output to cache files.
- Added cache-directory management via `.onLoad`: auto-creates `pkgcheck` and
  `static` sub-directories under the `rappdirs` application directory.

### CI and GitHub Actions

- Added `ci_badges` check: scrapes CI badges from the package README to
  determine CI provider status (GitHub Actions, Travis, etc.).
- Added `ci_results_gh` function: fetches GitHub Actions workflow run results
  directly from the GitHub API via `gert` and `gh`.
- Replaced all git remote shell commands with `gert` equivalents.
- Added `use_github_action_pkgcheck` stub and pkgdown GitHub Actions workflow.
- Added `check-other-OS` workflow to test on additional platforms.
- Added Docker image with full system dependencies (R, ctags, gtags, pandoc,
  visNetwork, spatial libraries); added `serve_api` function wrapping a
  `plumber` API.

### API / plumber endpoints (later moved to 'roreviewapi')

- Implemented plumber API endpoints: `check`, `goodpractice`, `srr`, `log`,
  `report`, `editorcheck`; later consolidated into `roreviewapi`.
- Added `dl_gh_repo` / `dl_repo` helpers for downloading package source from
  GitHub.

### Dependency changes

- Added imports: `cli`, `gert`, `gh`, `glue`, `pkgstats`, `rappdirs`,
  `rmarkdown`, `visNetwork`, `withr`.
- Removed `cloc`; removed plumber-related files; reduced exported fns.

### Documentation

- Added `CONTRIBUTING.md` (closes #35) and `CODE_OF_CONDUCT.md`.
- Added "Extending Checks" vignette describing how to add new `pkgchk_*`
  checks (closes #29, #36).
- Added "Installation of ctags" vignette.
- Added README sections on `checks_to_markdown`, the full list of checks, and
  Docker/API usage.
- Added `@maelle` (Maëlle Salmon) and `@assignUser` (Jacob Wujciak-Jens) as
package authors.

---

## 0.0.1

- Initial release of the package.
- Set up basic package structure: `pkgcheck()` main function, background-process
  helper (`bg` parameter), and `pg_report` entry point.
- Added `rappdirs`-based cache directory with `cache_dir` parameter and
  corresponding environment variable.
- Added GitHub Actions workflows (check, push-to-gitlab); configured Travis CI.
- Added `pkgdown` documentation action.
- Added `codemeta.json`.

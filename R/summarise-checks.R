# IMPORTANT: All sub-functions with `summarise_` prefixes summarise the actual
# checks, and include a return value specifying either "tick or cross", or just
# "cross only." The latter denotes checks which only appear when they fail,
# while the former appear in the summary list of green ticks required for a
# package to pass all checks.
#
# Any additional checks added must also specify `@return` values as either "tick
# or cross" (important checks which must be pased) or "cross only" (less
# important checks which only appear when failed).

#' Summarise main checklist items for editor report
#' @param checks Result of main \link{pkgcheck} function
#' @noRd
summarise_all_checks <- function (checks) {


    pkg_fns <- ls (envir = asNamespace ("pkgcheck"))

    output_fns <- gsub ("^output\\_pkgchk\\_", "",
                        grep ("^output\\_pkgchk\\_", pkg_fns, value = TRUE))
    out <- lapply (order_checks (output_fns),
                   function (i) summarise_check (checks, i))
    out <- do.call (c, out)

    gp <- summarise_gp_checks (checks)

    out <- c (out,
              gp$rcmd_errs,
              gp$rcmd_warns)

    checks_okay <- !any (grepl (symbol_crs (), out))
    if (!checks_okay) {
        out <- c (out,
                  "",
                  paste0 ("**Important:** All failing checks above ",
                          "must be addressed prior to proceeding"))
    }

    attr (out, "checks_okay") <- checks_okay

    return (out)
}

#' Function to specify the order in which checks appear in the summary method.
#'
#' @param fns List of output functions with prefixes `output_pkgchk_`, for which
#' order is to be established.
#' @return Modified version of input list with functions ordered in specified
#' sequence.
#' @noRd
order_checks <- function (fns) {

    ord <- c ("pkgname",
              "has_citation",
              "has_codemeta",
              "has_contrib",
              "uses_roxygen2",
              "has_url",
              "has_bugs",
              "has_vignette",
              "fns_have_exs",
              "global_assign",
              "ci",
              "covr",
              "has_scrap",
              "left_assign",
              "srr_missing",
              "srr_todo")

    fns <- fns [which (fns %in% ord)]
    fns <- fns [match (ord, fns)]

    return (fns)
}

#' Generic function to summarise checks based on result of corresponding
#' `output_pkgchk_` function.
#'
#' @param checks Full result of `pkgcheck()` call
#' @param what Name of check which must also correspond to an internal function
#' named `output_pkgchk_<name>`.
#' @return Check formatted to apepar in `summary` method
#' @noRd
summarise_check <- function (checks, what) {

    pkg_env <- asNamespace ("pkgcheck")
    pkg_fns <- ls (pkg_env)
    summary_fn <- paste0 ("output_pkgchk_", what)

    if (!summary_fn %in% pkg_fns)
        return (NULL)

    chk_summary <- do.call (summary_fn, list (checks), envir = pkg_env)

    res <- NULL

    if (sum (nchar (chk_summary$summary)) > 0L) {

        res <- paste0 ("- ",
                       ifelse (chk_summary$check_pass,
                               symbol_tck (),
                               symbol_crs ()),
                       " ",
                       chk_summary$summary)
    }

    return (res)
}

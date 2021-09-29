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


    gp <- summarise_gp_checks (checks)

    pkg_fns <- ls (as.environment ("package:pkgcheck"),
                   all.names = TRUE)

    check_fns <- gsub ("^pkgchk\\_", "",
                       grep ("^pkgchk\\_", pkg_fns, value = TRUE))
    output_fns <- gsub ("^output\\_pkgchk\\_", "",
                        grep ("^output\\_pkgchk\\_", pkg_fns, value = TRUE))
    out <- lapply (output_fns, function (i) summarise_check (checks, i))
    out <- do.call (c, out)

    out <- c (out,
              summarise_url_bugs (checks, "has_url"),
              summarise_url_bugs (checks, "has_bugs"),
              summarise_pkgname_chk (checks),
              summarise_ci_checks (checks),
              summarise_covr_checks (checks),
              gp$rcmd_errs,
              gp$rcmd_warns,
              # ---- Miscellaneous checks start here ---
              summarise_scrap_checks (checks),
              # ---- Miscellaneous checks end here ---
              summarise_srr_checks (checks))

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

# Generic function used to check components plus URL/BugRep fields
has_this <- function (checks, what, txt_yes, txt_no, txt_rest = NULL) {

    ret <- ifelse (checks [[what]],
                   paste0 ("- ", symbol_tck (),
                           " Package ", txt_yes),
                   paste0 ("- ", symbol_crs (),
                           " Package ", txt_no))
    if (!is.null (txt_rest))
        ret <- paste0 (ret, " ", txt_rest)

    return (ret)
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

    pkg_env <- as.environment ("package:pkgcheck")

    summary_fn <- get (paste0 ("output_pkgchk_", what),
                       envir = pkg_env)

    chk_summary <- do.call (summary_fn, list (checks))

    res <- NULL

    if (nchar (chk_summary$summary) > 0L) {

        res <- paste0 ("- ",
                       ifelse (chk_summary$check_pass,
                               symbol_tck (),
                               symbol_crs ()),
                       " ",
                       chk_summary$summary)
    }

    return (res)
}

#' Summarise both URL and BugReports fields from DESCRIPTION file
#' @return Tick or cross
#' @noRd
summarise_url_bugs <- function (checks, what = "has_url") {

    txt <- ifelse (what == "has_url",
                   "URL",
                   "BugReports")

    has_this (checks$checks, what,
              paste0 ("'DESCRIPTION' has a ", txt, " field."),
              paste0 ("'DESCRIPTION' does not have a ", txt, " field."))
}

#' @return tick or cross
#' @noRd
summarise_covr_checks <- function (checks) {

    if (methods::is (checks$checks$gp$covr, "try-error")) {

        res <- paste0 ("- ",
                       symbol_crs (),
                       " Package coverage failed")
    } else {

        coverage <- round (checks$checks$gp$covr$pct_by_line, digits = 1)

        if (coverage >= 75) {

            res <- paste0 ("- ",
                           symbol_tck (),
                           " Package coverage is ",
                           coverage,
                           "%.")

        } else {

            res <- paste0 ("- ",
                           symbol_crs (),
                           " Package coverage is ",
                           coverage,
                           "% (should be at least 75%).")
        }
    }

    return (res)
}

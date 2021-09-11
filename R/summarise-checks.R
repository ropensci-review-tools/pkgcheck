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

    out <- c (summarise_has_components (checks),
              summarise_has_contrib (checks),
              summarise_has_lifecycle (checks),
              summarise_fns_have_exs (checks),
              summarise_left_assign_chk (checks),
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

    ret <- ifelse (checks$file_list [[what]],
                   paste0 ("- ", symbol_tck (),
                           " Package ", txt_yes),
                   paste0 ("- ", symbol_crs (),
                           " Package ", txt_no))
    if (!is.null (txt_rest))
        ret <- paste0 (ret, " ", txt_rest)

    return (ret)
}

#' Summarise both URL and BugReports fields from DESCRIPTION file
#' @return Tick or cross
#' @noRd
summarise_url_bugs <- function (checks, what = "has_url") {

    txt <- ifelse (what == "has_url",
                   "URL",
                   "BugReports")

    has_this (checks, what,
              paste0 ("'DESCRIPTION' has a ", txt, " field"),
              paste0 ("'DESCRIPTION' does not have a ", txt, " field"))
}

#' Check presence of various required components
#' @param checks Result of main \link{pkgcheck} function
#' @return Test output with formatted check items as tick or cross.
#' @noRd
summarise_has_components <- function (checks) {

    uses_roxy <- has_this (checks, "uses_roxy",
                           "uses", "does not use", "'roxygen2'")
    has_lifecycle <- has_this (checks, "has_lifecycle",
                               "has", "does not have", "a life cycle statement")
    has_citation <- has_this (checks, "has_citation",
                             "has", "does not have", "a 'CITATION' file")
    has_codemeta <- has_this (checks, "has_codemeta",
                             "has", "does not have", "a 'codemeta.json' file")

    c (uses_roxy,
       #has_lifecycle, Uncomment to include lifecycle check
       has_citation,
       has_codemeta)
}

#' @return tick or cross
#' @noRd
summarise_covr_checks <- function (checks) {

    if (methods::is (checks$gp$covr, "try-error")) {

        res <- paste0 ("- ",
                       symbol_crs (),
                       " Package coverage failed")
    } else {

        coverage <- round (checks$gp$covr$pct_by_line, digits = 1)

        if (coverage >= 75) {

            res <- paste0 ("- ",
                           symbol_tck (),
                           " Package coverage is ",
                           coverage,
                           "%")

        } else {

            res <- paste0 ("- ",
                           symbol_crs (),
                           " Package coverage is ",
                           coverage,
                           "% (should be at least 75%)")
        }
    }

    return (res)
}

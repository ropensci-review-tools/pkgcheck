# IMPORTANT: All sub-functions with `collate_` prefixes summarise the actual
# checks, and include a return value specifying either "tick or cross", or just
# "cross only." The latter denotes checks which only appear when they fail,
# while the former appear in the summary list of green ticks required for a
# package to pass all checks.
#
# Any additional checks added must also specify `@return` values as either "tick
# or cross" (important checks which must be pased) or "cross only" (less
# important checks which only appear when failed).

#' Collate main checklist items for editor report
#' @param checks Result of main \link{pkgcheck} function
#' @noRd
collate_checks <- function (checks) {


    gp <- collate_gp_checks (checks)

    out <- c (collate_has_components (checks),
              collate_fn_exs (checks),
              collate_left_assign_chk (checks),
              collate_url_bugs (checks, "has_url"),
              collate_url_bugs (checks, "has_bugs"),
              collate_pkgname_chk (checks),
              collate_ci_checks (checks),
              collate_covr_checks (checks),
              gp$rcmd_errs,
              gp$rcmd_warns,
              collate_srr_checks (checks))

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

#' Collate both URL and BugReports fields from DESCRIPTION file
#' @return Tick or cross
#' @noRd
collate_url_bugs <- function (checks, what = "has_url") {

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
collate_has_components <- function (checks) {

    uses_roxy <- has_this (checks, "uses_roxy",
                           "uses", "does not use", "'roxygen2'")
    has_lifecycle <- has_this (checks, "has_lifecycle",
                               "has", "does not have", "a life cycle statement")
    has_contrib <- has_this (checks, "has_contrib",
                             "has", "does not have", "a 'contributing.md' file")
    has_citation <- has_this (checks, "has_citation",
                             "has", "does not have", "a 'CITATION' file")
    has_codemeta <- has_this (checks, "has_codemeta",
                             "has", "does not have", "a 'codemeta.json' file")

    c (uses_roxy,
       #has_lifecycle, Uncomment to include lifecycle check
       has_contrib,
       has_citation,
       has_codemeta)
}

#' Collate checks that all functions have examples
#'
#' @return tick or cross
#' @noRd
collate_fn_exs <- function (checks) {

    ifelse (all (checks$fn_exs),
            paste0 ("- ", symbol_tck (),
                    " All functions have examples"),
            paste0 ("- ", symbol_crs (),
                    " These funtions do not have examples: [",
                    paste0 (names (checks$fn_exs) [which (!checks$fn_exs)]),
                    "]"))

}

#' Collate checks that package name is available
#'
#' @return tick or cross
#' @noRd
collate_pkgname_chk <- function (checks) {

    if (checks$file_list$pkgname_available & !checks$file_list$pkg_on_cran) {

        res <- paste0 ("- ", symbol_tck (),
                       " Package name is available")

    } else if (checks$file_list$pkg_on_cran) {

        res <- paste0 ("- ", symbol_tck (),
                       " Package is already on CRAN")

    } else {

        res <- paste0 ("- ", symbol_crs (),
                       " Package name is not available (on CRAN)")
    }

    return (res)
}

#' @return cross only
#' @noRd
collate_left_assign_chk <- function (checks) {

    res <- NULL

    if (checks$left_assign$global) {

        res <- paste0 ("- ", symbol_crs (),
                       " Package uses global assignment operator ('<<-')")
    }

    if (length (which (checks$left_assign$usage == 0)) == 0) {

        la <- checks$left_assign$usage

        res <- c (res,
                  paste0 ("- ", symbol_crs (),
                          " Package uses inconsistent ",
                          "assignment operators (",
                          la [names (la) == "<-"], " '<-' and ",
                          la [names (la) == "="], " '=')"))
    }

    return (res)
}

#' Collate checks from continuous integration
#'
#' @return tick or cross
#' @noRd
collate_ci_checks <- function (checks) {

    if (length (checks$badges) == 0) {

        if (!checks$file_list$has_url) {

            res <- paste0 ("- ", symbol_crs (),
                           " Continuous integration checks unavailable ",
                           "(no URL in 'DESCRIPTION')")
        } else {

            res <- paste0 ("- ", symbol_crs (),
                           " Package has no continuous integration checks")
        }
    } else {

        res <- paste0 ("- ", symbol_tck (),
                       " Package has continuous integration checks")
    }

    return (res)
}

#' @return tick or cross
#' @noRd
collate_covr_checks <- function (checks) {

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

#' return tick or cross
#' @noRd
collate_gp_checks <- function (checks) {

    if (methods::is (checks$gp$rcmdcheck, "try-error")) {

        cond <- attr (checks$gp$rcmdcheck, "condition") # the error condition
        rcmd_errs <- paste0 ("- ",
                             symbol_crs (),
                             " R CMD check process failed with message: '",
                             cond$message,
                             "'")
        rcmd_warns <- NULL

    } else {

        nerr <- length (checks$gp$rcmdcheck$errors)
        if (nerr == 0) {

            rcmd_errs <- paste0 ("- ",
                                 symbol_tck (),
                                 " R CMD check found no errors")

        } else {

            rcmd_errs <- paste0 ("- ",
                                 symbol_crs (),
                                 " R CMD check found ",
                                 nerr,
                                 ifelse (nerr == 1,
                                         "error",
                                         "errors"))
        }

        nwarn <- length (checks$gp$rcmdcheck$warnings)
        if (nwarn == 0) {

            rcmd_warns <- paste0 ("- ",
                                  symbol_tck (),
                                  " R CMD check found no warnings")

        } else {

            rcmd_warns <- paste0 ("- ",
                                  symbol_crs (),
                                  " R CMD check found ",
                                  nwarn,
                                  ifelse (nwarn == 1,
                                          "warning",
                                          "warnings"))
        }
    }

    return (list (rcmd_errs = rcmd_errs,
                  rcmd_warns = rcmd_warns))
}

#' @return tick or cross (for 'srr' package only)
#' @noRd
collate_srr_checks <- function (checks) {

    res <- NULL

    if (!is.null (checks$srr)) {

        m <- checks$srr$message
        i <- which (nchar (m) > 0 & grepl ("[^\\s]*", m))

        res <- paste0 ("- ",
                       ifelse (checks$srr$okay,
                               symbol_tck (),
                               symbol_crs ()),
                       " ",
                       m [i [1]])
        res <- gsub (paste0 ("Package can not be submitted because ",
                             "the following standards are missing"),
                     "Statistical standards are missing",
                     res)
        res <- gsub (":$", "", res)
    }

    return (res)
}

#' @return cross only
#' @noRd
collate_scrap_checks <- function (checks) {

    ret <- NULL

    if (length (checks$scrap) > 0L) {

        ret <- paste0 ("- ", symbol_crs (),
                       " Package contained unexpected files")
    }

    return (ret)
}

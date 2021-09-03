
#' Collate main checklist items for editor report
#' @param checks Result of main \link{pkgcheck} function
#' @noRd
collate_checks <- function (checks) {

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


    fn_exs <- ifelse (all (checks$fn_exs),
                      paste0 ("- ", symbol_tck (),
                              " All functions have examples"),
                      paste0 ("- ", symbol_crs (),
                              " These funtions do not have examples: [",
                      paste0 (names (checks$fn_exs) [which (!checks$fn_exs)]),
                              "]"))

    has_url <- has_this (checks, "has_url",
                         "'DESCRIPTION' has a URL field",
                         "'DESCRIPTION' does not have a URL field")
    has_bugs <- has_this (checks, "has_bugs",
                      "'DESCRIPTION' has a BugReports field",
                      "'DESCRIPTION' does not have a BugReports field")

    gp <- gp_checks (checks)

    srr <- NULL
    if (!is.null (checks$srr)) {

        m <- checks$srr$message
        i <- which (nchar (m) > 0 & grepl ("[^\\s]*", m))

        srr <- paste0 ("- ",
                       ifelse (checks$srr$okay,
                               symbol_tck (),
                               symbol_crs ()),
                       " ",
                       m [i [1]])
        srr <- gsub (paste0 ("Package can not be submitted because ",
                             "the following standards are missing"),
                     "Statistical standards are missing",
                     srr)
        srr <- gsub (":$", "", srr)
    }

    out <- c (uses_roxy,
              has_contrib,
              has_citation,
              has_codemeta,
              fn_exs,
              collate_left_assign_chk (checks),
              has_url,
              has_bugs,
              collate_pkgname_chk (checks),
              collate_ci_checks (checks),
              collate_covr_checks (checks),
              gp$rcmd_errs,
              gp$rcmd_warns,
              srr)

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

# ci: continuous integration
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

gp_checks <- function (checks) {

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

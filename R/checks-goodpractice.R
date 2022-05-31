#' Get 'goodpractice' report on local source package
#'
#' This uses a caching system to only generate new report if repository has been
#' updated, otherwise it returns locally cached version. It has a prefix of
#' `pkgcheck_` and not `pkgchk_` because it is structured differently to
#' standard `pkgchk_` checks, and this avoids any method confusion.
#'
#' @inheritParams pkgcheck
#' @return A \pkg{goodpractice} report
#' @noRd
pkgcheck_gp_report <- function (path, use_cache, uses_renv) {

    cache_pkgcheck_component (path, use_cache, uses_renv, "goodpractice")
}

#' return tick or cross
#' @noRd
summarise_gp_checks <- function (checks) {

    if (methods::is (checks$goodpractice$rcmdcheck, "try-error")) {

        cond <- attr (
            checks$goodpractice$rcmdcheck,
            "condition"
        ) # the error condition
        rcmd_errs <- paste0 (
            "- ",
            symbol_crs (),
            " R CMD check process failed with message: '",
            cond$message,
            "'."
        )
        rcmd_warns <- NULL

    } else {

        nerr <- length (checks$goodpractice$rcmdcheck$errors)
        if (nerr == 0) {
            rcmd_errs <- paste0 (
                "- ",
                symbol_tck (),
                " R CMD check found no errors."
            )
        } else {
            rcmd_errs <- paste0 (
                "- ",
                symbol_crs (),
                " R CMD check found ",
                nerr,
                ifelse (nerr == 1,
                    " error.",
                    " errors."
                )
            )
        }

        nwarn <- length (checks$goodpractice$rcmdcheck$warnings)
        if (nwarn == 0) {
            rcmd_warns <- paste0 (
                "- ",
                symbol_tck (),
                " R CMD check found no warnings."
            )
        } else {
            rcmd_warns <- paste0 (
                "- ",
                symbol_crs (),
                " R CMD check found ",
                nwarn,
                ifelse (nwarn == 1,
                    " warning.",
                    " warnings."
                )
            )
        }
    }

    return (list (
        rcmd_errs = rcmd_errs,
        rcmd_warns = rcmd_warns
    ))
}

# No internal print method; pkgcheck uses default method from goodpractice

#' Convert goodpractice checks to markdown format
#'
#' @param checks Result of main \link{pkgcheck} function
#' @param control A named list of parameters potentially including
#' `cyclocomp_threshold`, `covr_threshold`, and `covr_digits`, where
#' reports are generated for cyclocomplexity values above threshold, and
#' coverage values below threshold (given as percentage). `digits` controls the
#' number of digits printed in coverage reports.
#' @noRd
gp_checks_to_md <- function (checks,
                             control = list (
                                 cyclocomp_threshold = 15,
                                 covr_threshold = 70,
                                 digits = 2
                             )) {

    gp <- extract_gp_components (checks$goodpractice)


    c (
        "",
        "#### 3b. `goodpractice` results",
        "",
        "",
        convert_gp_components (gp, control = control),
        ""
    )
}

# ------------------------------------------
# ---------- Additional functions ----------
# ------------------------------------------


extract_gp_components <- function (gp) {

    # -------------- covr:
    # from covr::print.coverage
    # https://github.com/r-lib/covr/blob/master/R/summary_functions.R
    covr <- list ()
    if (is.list (gp$covr)) {
        covr <- gp$covr$coverage
        group <- "filename"
        by <- "line"
        df <- covr::tally_coverage (covr, by = by)
        percents <- tapply (df$value,
            df [[group]],
            FUN = function (x) (sum (x > 0) / length (x)) * 100
        )
        overall_percentage <- covr::percent_coverage (df, by = by)
        covr <- c (
            package = overall_percentage,
            percents
        )
        covr <- data.frame (
            source = names (covr),
            percent = as.numeric (covr),
            stringsAsFactors = FALSE
        )
    }

    # -------------- cyclocomp:
    cyc <- gp$cyclocomp

    # -------------- lintr:
    lint_file <- vapply (gp$lintr, function (i) i$filename, character (1))
    lint_line <- vapply (gp$lintr, function (i) i$line_number, integer (1))
    lint_type <- vapply (gp$lintr, function (i) i$type, character (1))
    lint_message <- vapply (gp$lintr, function (i) i$message, character (1))
    lints <- data.frame (
        file = lint_file,
        line = lint_line,
        type = lint_type,
        message = lint_message,
        stringsAsFactors = FALSE
    )
    # lintr reports library calls in tests dir, which are okay
    index <- which (grepl ("^tests", lints$file) &
        grepl ("^Avoid library", lints$message))
    lints <- lints [-index, ]
    if (nrow (lints) == 0) {
        lints <- list ()
    }

    # -------------- rcmdcheck:
    r <- gp$rcmd
    rcmd <- list ()
    if (methods::is (r, "try-error")) {
        rcmd$errors <- paste0 (r)
    } else if (length (r) > 0) {
        if (length (r$errors) > 0) {
            rcmd$errors <- r$errors
        }
        if (length (r$warnings) > 0) {
            rcmd$warnings <- r$warnings
        }
        if (length (r$notes) > 0) {
            rcmd$notes <- r$notes
        }
        if (length (r$test_fail) > 0) {
            rcmd$test_fails <- r$test_fail
        } # note plural!
    }

    # -------------- any other components which fail:
    checks <- vapply (
        gp$checks, function (i) {
            ret <- TRUE
            if (is.logical (i) & length (i) == 1) {
                ret <- i
            }
            return (ret)
        },
        logical (1)
    )
    fails <- names (checks [which (!checks)])
    if (length (fails) > 0) {
        rcmd$check_fails <- fails
    }

    # return result
    res <- list (
        package = unname (gp$package),
        rcmd = rcmd,
        covr = covr,
        cyclocomp = cyc,
        lint = lints
    )
    res <- res [which (vapply (res, length, integer (1)) > 0)]
}

#' Convert \pkg{goodpractice} components into templated report
#'
#' @param x List of components of \pkg{goodpractice} report
#' @param control A named list of parameters potentially including
#' `cyclocomp_threshold`, `covr_threshold`, and `covr_digits`, where
#' reports are generated for cyclocomplexity values above threshold, and
#' coverage values below threshold (given as percentage). `digits` controls the
#' number of digits printed in coverage reports.
#' @return Markdown-formatted report of contents of `x`
#' @noRd
convert_gp_components <- function (x,
                                   control = list (
                                       cyclocomp_threshold = 15,
                                       covr_threshold = 70,
                                       digits = 2
                                   )) {

    rcmd <- rcmd_report (x)

    covr <- covr_report (x, control)

    cycl <- cyclo_report (x, control)

    lint <- lintr_report (x)

    return (c (rcmd, covr, cycl, lint))
}


rcmd_report <- function (x) {

    ret <- c (
        paste0 (
            "#### `R CMD check` with [rcmdcheck]",
            "(https://r-lib.github.io/rcmdcheck/)"
        ),
        ""
    )

    if (!"rcmd" %in% names (x)) {
        return (c (
            ret,
            "rcmdcheck found no errors, warnings, or notes",
            ""
        ))
    }

    rcmd <- x$rcmd
    if (methods::is (rcmd, "try-error")) {
        return (rcmd)
    }

    ret <- c (ret, dump_one_rcmd_type (rcmd, "errors"))
    ret <- c (ret, dump_one_rcmd_type (rcmd, "warnings"))
    ret <- c (ret, dump_one_rcmd_type (rcmd, "notes"))
    ret <- c (ret, dump_one_rcmd_type (rcmd, "test_fails"))
    ret <- c (ret, dump_one_rcmd_type (rcmd, "check_fails"))

    return (c (ret, ""))
}

dump_one_rcmd_type <- function (rcmd, type = "errors") {

    ret <- NULL

    if (!type %in% names (rcmd)) {
        return (ret)
    }

    msg <- paste0 (
        "R CMD check generated the following ",
        gsub ("s$", "", type)
    )
    if (length (rcmd [[type]]) > 1) {
        msg <- paste0 (msg, "s")
    }
    msg <- paste0 (msg, ":")

    ret <- c (
        ret,
        msg,
        ""
    )

    for (i in seq.int (rcmd [[type]])) {
        ret <- c (
            ret,
            paste0 (i, ". ", rcmd [[type]] [i])
        )
    }

    ret <- c (ret, "")

    return (ret)
}

covr_report <- function (x,
                         control = list (
                             cyclocomp_threshold = 15,
                             covr_threshold = 70,
                             digits = 2
                         )) {

    res <- c (
        "#### Test coverage with [covr](https://covr.r-lib.org/)",
        ""
    )

    if (!"covr" %in% names (x)) {
        return (c (
            res,
            "ERROR: Test Coverage Failed",
            ""
        ))
    }
    if (methods::is (x$covr, "try-error")) {
        return (c (
            res,
            paste0 (x$covr),
            ""
        ))
    }

    if ("covr_threshold" %in% names (control)) {
        covr_threshold <- control$covr_threshold
    } else {
        covr_threshold <- 70
    }

    if ("digits" %in% names (control)) {
        digits <- control$digits
    } else {
        digits <- 2
    }

    pkg_line <- which (x$covr$source == "package")
    pkg_cov <- x$covr$percent [pkg_line]

    covr <- x$covr [-pkg_line, ]

    covr <- covr [covr$percent < covr_threshold, ]

    res <- c (
        res,
        paste0 ("Package coverage: ", round (pkg_cov, digits = digits))
    )

    if (pkg_cov >= covr_threshold) {
        return (c (res, ""))
    }

    if (nrow (covr) > 0) {
        res <- c (
            res,
            "",
            "The following files are not completely covered by tests:",
            "",
            "file | coverage",
            "--- | ---"
        )
        for (i in seq.int (nrow (covr))) {
            res <- c (
                res,
                paste0 (
                    covr$source [i],
                    " | ",
                    round (covr$percent [i], digits = digits),
                    "%"
                )
            )
        }
    }
    res <- c (res, "")

    return (res)
}

cyclo_report <- function (x,
                          control = list (
                              cyclocomp_threshold = 15,
                              covr_threshold = 70,
                              digits = 2
                          )) {

    if ("cyclocomp_threshold" %in% names (control)) {
        cyc_thr <- control$cyclocomp_threshold
    } else {
        cyc_thr <- 15
    }

    cyc <- x$cyclocomp

    ret <- c (
        paste0 (
            "#### Cyclocomplexity with [cyclocomp]",
            "(https://github.com/MangoTheCat/cyclocomp)"
        ),
        ""
    )

    if (methods::is (cyc, "try-error")) {
        ret <- c (ret, paste0 (cyc))
    } else {
        cyc <- cyc [cyc$cyclocomp >= cyc_thr, ]

        if (nrow (cyc) == 0) {
            ret <- c (
                ret,
                paste0 ("No functions have cyclocomplexity >= ", cyc_thr)
            )
        } else {
            msg <- "The following function"
            if (nrow (cyc) > 1) {
                msg <- paste0 (msg, "s")
            }
            msg <- paste0 (msg, " have cyclocomplexity >= ", cyc_thr, ":")
            ret <- c (
                ret,
                msg,
                "",
                "function | cyclocomplexity",
                "--- | ---"
            )

            for (i in seq.int (nrow (cyc))) {
                ret <- c (
                    ret,
                    paste0 (cyc$name [i], " | ", cyc$cyclocomp [i])
                )
            }
        }
    }

    ret <- c (ret, "")

    return (ret)
}

lintr_report <- function (x) {

    ret <- c (
        paste0 (
            "#### Static code analyses with [lintr]",
            "(https://github.com/jimhester/lintr)"
        ),
        ""
    )

    if (is.null (x$lint)) {
        return (c (
            ret,
            paste0 (
                "[lintr](https://github.com/jimhester/lintr) ",
                "found no issues with this package!"
            ),
            ""
        ))
    }

    msgs <- table (x$lint$message)
    msgs <- data.frame (
        message = names (msgs),
        n = as.integer (msgs),
        stringsAsFactors = FALSE
    )

    ret <- c (
        ret,
        paste0 (
            "[lintr](https://github.com/jimhester/lintr) ",
            "found the following ",
            sum (msgs$n),
            " potential issues:"
        ),
        "",
        "message | number of times",
        "--- | ---"
    )
    for (i in seq.int (nrow (msgs))) {
        ret <- c (
            ret,
            paste0 (msgs$message [i], " | ", msgs$n [i])
        )
    }

    return (c (ret, ""))
}

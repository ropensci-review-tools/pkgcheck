#' Get 'goodpractice' report on local source package
#'
#' This uses a caching system to only generate new report if repository has been
#' updated, otherwise it returns locally cached version.
#'
#' @param u URL
#' @param local_repo Path to local source of repository
#' @return A \pkg{goodpractice} report
#' @export
get_gp_report <- function (u, local_repo) {

    repo <- tail (strsplit (u, "/") [[1]], 1)
    org <- tail (strsplit (u, "/") [[1]], 2) [1]

    cache_dir <- Sys.getenv ("cache_dir")

    # check whether gp is cached:
    cmt <- pkgreport::get_latest_commit (org = org, repo = repo)
    fname <- paste0 (repo, "_", substring (cmt$oid, 1, 8))
    gp_cache_dir <- file.path (cache_dir, "gp_reports")
    gp_cache_file <- file.path (gp_cache_dir, fname)

    if (file.exists (gp_cache_file)) {

        gp <- readRDS (gp_cache_file)

    } else {

        if (!file.exists (gp_cache_dir))
            dir.create (gp_cache_dir, recursive = TRUE)

        sys_deps <- Sys.getenv ("pkgrep_sys_deps")
        if (sys_deps) {
            sys_config <- list (os = Sys.getenv ("pkgrep_os"),
                                os_release = Sys.getenv ("pkgrep_os_release"))
            sr <- remotes::system_requirements (os = sys_config$os,
                                                os_release = sys_config$os_release,
                                                path = local_repo)
            for (s in sr)
                system (s)
        }

        withr::with_temp_libpaths ({
            remotes::install_local (local_repo,
                                    upgrade = "never",
                                    dependencies = TRUE)
            gp <- goodpractice::goodpractice (local_repo)
        })

        saveRDS (gp, gp_cache_file)
    }

    return (gp)
}

#' process 'goodpractice' report
#'
#' Extract various components from 'goodpractice' report and convert into format
#' to be returned by endpoint
#' @param gp A 'goodpractice' object as returned by \pkg{goodpractice}
#' @param control A named list of parameters potentially including
#' `cyclocomp_threshold`, `covr_threshold`, and `covr_digits`, where
#' reports are generated for cyclocomplexity values above threshold, and
#' coverage values below threshold (given as percentage). `digits` controls the
#' number of digits printed in coverage reports.
#' @return Markdown-formatted report
#' @export
process_gp <- function (gp,
                        control = list (cyclocomp_threshold = 15,
                                        covr_threshold = 70,
                                        digits = 2)) {

    gp <- extract_gp_components (gp)

    return (convert_gp_components (gp, control = control))
}


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
                            df[[group]],
                            FUN = function (x) (sum (x > 0) / length (x)) * 100)
        overall_percentage <- covr::percent_coverage(df, by = by)
        covr <- c (package = overall_percentage,
                   percents)
        covr <- data.frame (source = names (covr),
                            percent = as.numeric (covr))
    }

    # -------------- cyclocomp:
    cyc <- gp$cyclocomp

    # -------------- lintr:
    lint_file <- vapply (gp$lintr, function (i) i$filename, character (1))
    lint_line <- vapply (gp$lintr, function (i) i$line_number, integer (1))
    lint_type <- vapply (gp$lintr, function (i) i$type, character (1))
    lint_message <- vapply (gp$lintr, function (i) i$message, character (1))
    lints <- data.frame (file = lint_file,
                         line = lint_line,
                         type = lint_type,
                         message = lint_message,
                         stringsAsFactors = FALSE)
    # lintr reports library calls in tests dir, which are okay
    index <- which (grepl ("^tests", lints$file) &
                    grepl ("^Avoid library", lints$message))
    lints <- lints [-index, ]
    if (nrow (lints) == 0)
        lints <- list ()

    # -------------- rcmdcheck:
    r <- gp$rcmdcheck
    rcmd <- list ()
    if (length (r$errors) > 0)
        rcmd$errors <- r$errors
    if (length (r$warnings) > 0)
        rcmd$warnings <- r$warnings
    if (length (r$notes) > 0)
        rcmd$notes <- r$notes
    if (length (r$test_fail) > 0)
        rcmd$test_fails <- r$test_fail # note plural!

    # -------------- any other components which fail:
    checks <- vapply (gp$checks, function (i) {
                         ret <- TRUE
                         if (is.logical (i) & length (i) == 1)
                             ret <- i
                         return (ret)   },
                         logical (1))
    fails <- names (checks [which (!checks)])
    if (length (fails) > 0)
        rcmd$check_fails <- fails

    # return result
    res <- list (package = unname (gp$package),
                 rcmd = rcmd,
                 covr = covr,
                 cyclocomp = cyc,
                 lint = lints)
    res <- res [which (vapply (res, length, integer (1)) > 0)]
}

#' Convert \pkg{goodpractice} components into templated report
#'
#' @inheritParams process_gp
#' @param x List of components of \pkg{goodpractice} report
#' @return Markdown-formatted report of contents of `x`
#' @export
convert_gp_components <- function (x,
                                   control = list (cyclocomp_threshold = 15,
                                                   covr_threshold = 70,
                                                   digits = 2)) {

    rcmd <- rcmd_report (x)
    covr <- covr_report (x, control)
    cycl <- cyclo_report (x, control)

    res <- c (rcmd, covr, cycl)

    if (length (res) == 0)
        res <- "goodpractice found no issues; this package is awesome!"

    return (res)
}

rcmd_report <- function (x) {

    ret <- NULL

    if (!"rcmd" %in% names (x))
        return (ret)

    ret <- c ("### R CMD check",
              "")

    rcmd <- x$rcmd

    ret <- c (ret, dump_one_rcmd_type (rcmd, "errors"))
    ret <- c (ret, dump_one_rcmd_type (rcmd, "warnings"))
    ret <- c (ret, dump_one_rcmd_type (rcmd, "notes"))
    ret <- c (ret, dump_one_rcmd_type (rcmd, "test_fails"))
    ret <- c (ret, dump_one_rcmd_type (rcmd, "check_fails"))

    return (ret)
}

dump_one_rcmd_type <- function (rcmd, type = "errors") {

    ret <- NULL

    if (!type %in% names (rcmd))
        return (ret)

    msg <- paste0 ("R CMD check generated the following ",
                   gsub ("s$", "", type))
    if (length (rcmd [[type]]) > 1)
        msg <- paste0 (msg, "s")
    msg <- paste0 (msg, ":")

    ret <- c (ret,
              msg,
              "")

    for (i in seq.int (rcmd [[type]])) {
        ret <- c (ret,
                  paste0 (i, ". ", rcmd [[type]] [i]))
    }

    ret <- c (ret, "")

    return (ret)
}

covr_report <- function (x,
                         control = list (cyclocomp_threshold = 15,
                                         covr_threshold = 70,
                                         digits = 2)) {

    if (!"covr" %in% names (x))
        return (c ("### Test Coverage",
                   "",
                   "ERROR: Test Coverage Failed",
                   ""))

    if ("covr_threshold" %in% names (control))
        covr_threshold <- control$covr_threshold
    else
        covr_threshold <- 70

    if ("digits" %in% names (control))
        digits <- control$digits
    else
        digits <- 2

    pkg_line <- which (x$covr$source == "package")
    pkg_cov <- x$covr$percent [pkg_line]

    if (pkg_cov >= covr_threshold)
        return (NULL)

    covr <- x$covr [-pkg_line, ]

    covr <- covr [covr$percent < covr_threshold, ]

    res <- c ("### Test Coverage",
              "",
              paste0 ("Package: ", round (pkg_cov, digits = digits)))

    if (nrow (covr) > 0) {
        res <- c (res,
                  "",
                  "The following files are not completely covered by tests:",
                  "",
                  "file | coverage",
                  "--- | ---")
        for (i in seq.int (nrow (covr))) {
            res <- c (res,
                      paste0 (covr$source [i],
                              " | ",
                              round (covr$percent [i], digits = digits),
                              "%"))
        }
    }
    res <- c (res, "")

    return (res)
}

cyclo_report <- function (x,
                          control = list (cyclocomp_threshold = 15,
                                          covr_threshold = 70,
                                          digits = 2)) {

    if ("cyclocomp_threshold" %in% names (control))
        cyc_thr <- control$cyclocomp_threshold
    else
        cyc_thr <- 15

    cyc <- x$cyclocomp [x$cyclocomp$cyclocomp >= cyc_thr, ]

    ret <- NULL

    if (nrow (cyc) == 0)
        return (ret)

    msg <- "The following function"
    if (nrow (cyc) > 1)
        msg <- paste0 (msg, "s")
    msg <- paste0 (msg, " have cyclocomplexity >= ", cyc_thr, ":")
    res <- c ("### Cyclocomplexity",
              "",
              msg,
              "",
              "function | cyclocomplexity",
              "--- | ---")

    for (i in seq.int (nrow (cyc)))
        res <- c (res,
                  paste0 (cyc$name [i], " | ", cyc$cyclocomp [i]))

    res <- c (res, "")

    return (res)
}

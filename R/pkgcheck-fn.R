
#' Generate report on package compliance with rOpenSci Statistical Software
#' requirements
#'
#' @param path Path to local repository
#' @param goodpractice If `FALSE`, skip goodpractice checks. May be useful in
#' development stages to more quickly check other aspects.
#' @param extra_env Additional environments from which to collate checks. Other
#' package names may be appended using `c`, as in `c(.GlobalEnv, "mypkg")`.
#' @return A `pkgcheck` object detailing all package assessments automatically
#' applied to packages submitted for peer review.
#' @family pkgcheck_fns
#' @export
pkgcheck <- function (path = ".", goodpractice = TRUE, extra_env = .GlobalEnv) {

    options (pkgcheck_extra_env = extra_env)

    path <- convert_path (path)

    if (checks_running_in_bg (path)) {
        stop ("Checks are still running in background process.")
    }

    # Ensure that ctags works properly (#54):
    if (interactive ()) {
        if (!suppressMessages (pkgstats::ctags_test ())) {
            stop (
                "The 'pkgstats' package requires 'ctags' which does ",
                "not seem to be installed correctly.\nSee ",
                "https://docs.ropensci.org/pkgstats/#installation",
                " for details on how to install 'ctags'."
            )
        }
    }

    s <- pkgstats_info (path)

    if (nrow (s$stats$objects) == 0L) {
        # There are no R objects/fns; current goodpractice (1.0.2.9000) fails
        goodpractice <- FALSE
    }

    checks <- pkgcheck_object ()
    checks$pkg <- s$out [c (
        "name", "path", "version", "url",
        "BugReports", "license", "summary",
        "dependencies"
    )]

    if ("srr" %in% names (s$out)) {
        checks$info <- s$out [c ("git", "srr", "pkgstats")]
    } else {
        checks$info <- s$out [c ("git", "pkgstats")]
    }

    checks$info$pkgdown_concepts <- pkginfo_pkgdown (path)

    checks$info$network_file <- fn_call_network (s)

    if (goodpractice) {
        checks$goodpractice <- pkgcheck_gp_report (path)
    } else {
        checks$goodpractice <- NULL
    }

    u <- pkginfo_url_from_desc (path, type = "URL")
    # hard-code to extract github URLs only:
    if (!grepl ("github", u, ignore.case = TRUE)) {
        u <- pkginfo_url_from_desc (path, type = "BugReports")
        if (grepl ("issues(\\/?)$", u)) {
            u <- gsub ("issues(\\/?)$", "", u)
        }
    }

    checks$info$badges <- list ()
    has_token <- length (get_gh_token ()) > 0L
    if (nzchar (u) & has_token) {
        checks$info$badges <- pkgchk_ci_badges (u)
        if (grepl ("github", u)) { # now redundant - remove!
            checks$info$github_workflows <- ci_results_gh (path)
        }
    }

    checks$meta <- version_info (is.null (checks$info$srr))

    checks$checks <- collate_checks (checks)

    stopfile <- Sys.getenv ("PKGCHECK_PXBG_STOP")
    if (stopfile != "") {
        writeLines ("process stopped", con = stopfile)
    }

    return (checks)
}

pkgcheck_object <- function () {

    out <- list (
        pkg = NULL,
        info = NULL,
        checks = NULL,
        meta = NULL
    )

    class (out) <- append ("pkgcheck", class (out))

    return (out)
}

checks_running_in_bg <- function (path) {

    stopvar <- Sys.getenv ("PKGCHECK_PXBG_STOP")
    if (Sys.getenv ("PKGCHECK_BG") != "") {
        stopvar <- ""
    }

    logfiles <- logfile_names (path)
    stopfile <- gsub (
        "\\_stdout$", "_stop",
        logfiles$stdout
    )

    return (stopvar == stopfile &&
        !file.exists (stopfile))
}

pkgstats_info <- function (path) {

    s <- suppressWarnings (cache_pkgstats_component (path, "pkgstats"))
    s$path <- path

    out <- list ()
    out$name <- pkginfo_pkg_name (s)
    out$path <- path
    out$version <- pkginfo_pkg_version (s)
    out$url <- pkginfo_url_from_desc (path, type = "URL")
    out$BugReports <- pkginfo_url_from_desc (path, type = "BugReports") # nolint
    out$license <- pkginfo_pkg_license (s)

    out$summary <- pkginfo_pkgstats_summary (s)
    out$dependencies <- parse_pkg_deps (s)

    out$git <- pkginfo_git_info (path)

    out$srr <- pkginfo_srr_report (path)

    out$pkgstats <- fmt_pkgstats_info (s)

    return (list (
        stats = s,
        out = out
    ))
}

#' Parse items of the "desc" part of `pkgstats` output
#'
#' @param s Result of `pkgstats::pkgstats()` call.
#' @noRd
parse_pkg_deps <- function (s) {

    fields <- c ("depends", "imports", "suggests", "linking_to")

    d <- lapply (fields, function (i) {
        cbind (
            i,
            strsplit (s$desc [[i]], ",\\s*") [[1]]
        )
    })

    d <- do.call (rbind, d)

    data.frame (
        type = d [, 1],
        package = d [, 2],
        stringsAsFactors = FALSE
    )
}

#' Format \pkg{pkgstats} data
#' @param s Output of \pkg{pkgstats} call.
#' @return Report as formatted string
#' @noRd
fmt_pkgstats_info <- function (s) {

    s_summ <- pkgstats::pkgstats_summary (s)
    attr (s_summ, "path") <- s$path
    stat_chks <- stats_checks (s_summ)
    languages <- attr (stat_chks, "language")
    # ignore large numbers of files:
    stat_chks$noteworthy [grepl ("^files\\_", stat_chks$measure) &
        stat_chks$percentile > 0.5] <- FALSE
    # is_noteworthy <- any (stat_chks$noteworthy)
    stat_chks$percentile <- 100 * stat_chks$percentile
    stat_chks$noteworthy [which (!stat_chks$noteworthy)] <- ""

    attr (stat_chks, "language") <- languages

    return (stat_chks)
}

#' Collates results of all main `pkgchk_` functions
#'
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @return The contents of the "checks" items of the main `pkgcheck` object.
#' @noRd
collate_checks <- function (checks) {

    pkg_fns <- ls (envir = asNamespace ("pkgcheck"))
    check_fns <- grep ("^pkgchk\\_", pkg_fns, value = TRUE)
    exclude_these <- "ci\\_badges|srr"
    check_fns <- check_fns [which (!grepl (exclude_these, check_fns))]

    res <- lapply (check_fns, function (i) {
        do.call (i, list (checks))
    })
    names (res) <- gsub ("^pkgchk\\_", "", check_fns)

    extra_chks <- collate_extra_env_checks (checks)

    return (c (res, extra_chks))
}

collate_extra_env_checks <- function (checks) {

    extra_env <- options ("pkgcheck_extra_env") [[1]]
    if (is.null (extra_env)) {
        return (NULL)
    }

    if (!is.list (extra_env)) {
        extra_env <- list (extra_env)
    }

    chks <- lapply (extra_env, function (i) {
        i <- env2namespace (i) # in R/utils.R
        fns <- grep ("^pkgchk\\_", ls (i), value = TRUE)
        out <- lapply (fns, function (j) {
            do.call (j, list (checks), envir = i)
        })
        names (out) <- gsub ("^pkgchk\\_", "", fns)

        return (out)
    })
    chks <- unlist (chks)

    not_empty <- vapply (
        chks,
        function (i) length (i) > 0L,
        logical (1)
    )

    return (chks [which (not_empty)])
}

version_info <- function (nosrr) {

    pkgs <- c ("pkgstats", "pkgcheck")
    if (!nosrr) {
        pkgs <- c (pkgs, "srr")
    }

    vapply (
        pkgs, function (i) {
            paste0 (utils::packageVersion (i))
        },
        character (1)
    )
}

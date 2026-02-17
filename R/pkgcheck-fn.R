#' Generate report on package compliance with rOpenSci Statistical Software
#' requirements
#'
#' @param path Path to local repository
#' @param goodpractice If `FALSE`, skip most goodpractice checks except
#' \pkg{lintr} and 'DESCRIPTION' checks. May be useful in development stages to
#' more quickly check other aspects.
#' @param use_cache Checks are cached for rapid retrieval, and only re-run if
#' the git hash of the local repository changes. Setting `use_cache` to `FALSE`
#' will force checks to be re-run even if the git hash has not changed.
#' @param extra_env Additional environments from which to collate checks. Other
#' package names may be appended using `c`, as in `c(.GlobalEnv, "mypkg")`.
#' @return A `pkgcheck` object detailing all package assessments automatically
#' applied to packages submitted for peer review.
#' @family pkgcheck_fns
#' @export
#' @examples
#' \dontrun{
#' checks <- pkgcheck ("/path/to/my/package") # default full check
#' summary (checks)
#' # Or to run only checks implemented in 'pkgcheck' and not the
#' # additional \pkg{goodpractice} checks:
#' checks <- pkgcheck ("/path/to/my/package", goodpractice = FALSE)
#' summary (checks)
#' }
pkgcheck <- function (path = ".", goodpractice = TRUE,
                      use_cache = TRUE, extra_env = .GlobalEnv) {

    options (pkgcheck_extra_env = extra_env)

    path_in <- path
    path <- convert_path (path)

    if (checks_running_in_bg (path)) {
        stop ("Checks are still running in background process.")
    }

    # Ensure that ctags works properly (#54):
    if (interactive ()) {
        tryCatch (
            pkgstats::ctags_test (),
            error = function (e) {
                stop (
                    "The 'pkgstats' package requires 'ctags' which does ",
                    "not seem to be installed correctly.\nSee ",
                    "https://docs.ropensci.org/pkgstats/articles/installation.html",
                    " for details on how to install 'ctags'."
                )
            }
        )
    }

    s <- pkgstats_info (path, use_cache)

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
    checks$pkg$repo_path <- path_in

    ex <- s$stats$external_calls
    checks$pkg$external_calls <- sort (table (ex$package), decreasing = TRUE)

    pkgs <- sort (table (ex$package), decreasing = TRUE)
    checks$pkg$external_fns <- lapply (names (pkgs), function (i) {
        sort (table (ex$call [which (ex$package == i)]), decreasing = TRUE)
    })
    names (checks$pkg$external_fns) <- names (pkgs)

    info_items <- c ("fn_names", "git", "pkgstats")
    if ("srr" %in% names (s$out)) {
        info_items <- c (info_items, "srr")
    }
    checks$info <- s$out [info_items]

    checks$info$pkgdown_concepts <- pkginfo_pkgdown (path)
    checks$info$network_file <- fn_call_network (s)
    checks$info$renv_activated <- pkginfo_renv_activated (path)

    checks$goodpractice <- pkgcheck_gp_report (
        path,
        gp_full = goodpractice,
        use_cache = use_cache,
        renv_activated = checks$info$renv_activated
    )

    from_cache <- s$from_cache || attr (checks$goodpractice, "from_cache")
    if (from_cache) {
        cli::cli_alert_info (paste0 (
            "To re-generate, call 'pkgcheck' function with ",
            "'use_cache = FALSE', or delete the cached files."
        ))
    }
    attr (checks$goodpractice, "from_cache") <- NULL

    u <- pkginfo_url_from_desc (path, type = "URL")
    # hard-code to extract github URLs only:
    if (!grepl ("github", u, ignore.case = TRUE) |
        grepl ("github\\.io", u, ignore.case = TRUE)) {
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
            checks$info$github_workflows <- suppressWarnings (
                tryCatch (ci_results_gh (path), error = function (e) NULL)
            )
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

#' Collates results of all main `pkgchk_` functions
#'
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @return The contents of the "checks" items of the main `pkgcheck` object.
#' @noRd
collate_checks <- function (checks) {

    pkg_fns <- ls (envir = asNamespace ("pkgcheck"))
    check_fns <- grep ("^pkgchk\\_", pkg_fns, value = TRUE)
    exclude_these <- "ci\\_badges"
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

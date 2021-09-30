
#' Generate report on package compliance with rOpenSci Statistical Software
#' requirements
#'
#' @param path Path to local repository
#' @param extra_env Additional environments from which to collate checks. Other
#' package names may be appended using `c`, as in `c(.GlobalEnv, "mypkg")`.
#' @return A `pkgcheck` object detailing all package assessments automatically
#' applied to packages submitted for peer review.
#' @family pkgcheck_fns
#' @export
pkgcheck <- function (path = ".", extra_env = .GlobalEnv) {

    path <- convert_path (path)

    if (checks_running_in_bg (path))
        stop ("Checks are still running in background process.")

    # Ensure that ctags works properly (#54):
    if (interactive ())
        if (!suppressMessages (pkgstats::ctags_test ())) {

            stop ("The 'pkgstats' package requires 'ctags' which does ",
                  "not seem to be installed correctly.\nSee ",
                  "https://docs.ropensci.org/pkgstats/#installation",
                  " for details on how to install 'ctags'.")
        }

    s <- pkgstats_info (path)

    checks <- pkgcheck_object ()
    checks$package <- s$out [c ("package", "path", "version", "url",
                                "BugReports", "license", "summary",
                                "dependencies")]
    names (checks$package) [1] <- "name"

    checks$info <- s$out [c ("git", "srr", "pkgstats")]

    checks$info$network_file <- fn_call_network (s)

    checks$goodpractice <- pkgcheck_gp_report (path)

    checks$checks <- collate_checks (checks, extra_env)

    u <- pkginfo_url_from_desc (path)
    checks$info$badges <- list ()
    if (length (u) > 0L) {
        checks$info$badges <- pkgchk_ci_badges (u)
        if (!is.null (checks$info$badges)) {
            if (any (grepl ("github", checks$info$badges))) {
                checks$info$github_workflows <- ci_results_gh (path)
            }
        }
    }

    checks$meta <- version_info (is.null (checks$info$srr))

    stopfile <- Sys.getenv ("PKGCHECK_PXBG_STOP")
    if (stopfile != "") {
        writeLines ("process stopped", con = stopfile)
    }

    return (checks)
}

pkgcheck_object <- function () {

    out <- list (package = NULL,
                 info = NULL,
                 checks = NULL,
                 meta = NULL)

    class (out) <- append ("pkgcheck", class (out))

    return (out)
}

checks_running_in_bg <- function (path) {

    stopvar <- Sys.getenv ("PKGCHECK_PXBG_STOP")
    if (Sys.getenv ("PKGCHECK_BG") != "")
        stopvar <- ""

    logfiles <- logfile_names (path)
    stopfile <- gsub ("\\_stdout$", "_stop",
                      logfiles$stdout)

    return (stopvar == stopfile &&
            !file.exists (stopfile))
}

pkgstats_info <- function (path) {

    s <- suppressWarnings (pkgstats::pkgstats (path))
    s$path <- path

    out <- list ()
    out$package <- pkginfo_pkg_name (s)
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

    return (list (stats = s,
                  out = out))
}

#' Parse items of the "desc" part of `pkgstats` output
#'
#' @param s Result of `pkgstats::pkgstats()` call.
#' @noRd
parse_pkg_deps <- function (s) {

    fields <- c ("depends", "imports", "suggests", "linking_to")

    d <- lapply (fields, function (i)
                 cbind (i,
                        strsplit (s$desc [[i]], ",\\s*") [[1]]))

    d <- do.call (rbind, d)

    data.frame (type = d [, 1],
                package = d [, 2])
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
    #is_noteworthy <- any (stat_chks$noteworthy)
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
collate_checks <- function (checks, extra_env = .GlobalEnv) {

    pkg_fns <- ls (envir = asNamespace ("pkgcheck"))
    check_fns <- grep ("^pkgchk\\_", pkg_fns, value = TRUE)
    exclude_these <- "ci\\_badges|srr"
    check_fns <- check_fns [which (!grepl (exclude_these, check_fns))]

    res <- lapply (check_fns, function (i)
                          do.call (i, list (checks)))
    names (res) <- gsub ("^pkgchk\\_", "", check_fns)

    if (!methods::is (extra_env, "list"))
        extra_env <- list (extra_env)

    extra_chks <- lapply (extra_env, function (i) {
                              if (!methods::is (i, "environment")) {
                                  s <- search ()
                                  i <- s [grep (i, s)]
                                  if (length (i) != 1L)
                                      i <- NULL
                                  else {
                                      pkg <- gsub ("package\\:", "", i)
                                      i <- asNamespace (pkg)
                                  }
                              }
                              fns <- grep ("^pkgchk\\_", ls (i),
                                           value = TRUE)
                              out <- lapply (fns, function (j)
                                             do.call (j, list (checks),
                                                      envir = i))
                              names (out) <- gsub ("^pkgchk\\_", "",
                                                   fns)

                              return (out)
                })
    not_empty <- vapply (extra_chks,
                         function (i) length (i) > 0L,
                         logical (1))
    extra_chks <- extra_chks [which (not_empty)]

    return (c (res, extra_chks))
}

version_info <- function (nosrr) {

    pkgs <- c ("pkgstats", "pkgcheck")
    if (!nosrr)
        pkgs <- c (pkgs, "srr")

    vapply (pkgs, function (i)
            paste0 (utils::packageVersion (i)),
            character (1))
}

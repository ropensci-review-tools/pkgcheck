
#' Generate report on package compliance with rOpenSci Statistical Software
#' requirements
#'
#' @param path Path to local repository
#' @return A `pkgcheck` object detailing all package assessments automatically
#' applied to packages submitted for peer review.
#' @family pkgcheck_fns
#' @export
pkgcheck <- function (path) {

    if (checks_running_in_bg (path))
        stop ("Checks are still running in background process.")

    s <- pkgstats_checks (path)
    out <- s$out

    out$network_file <- fn_call_network (s)

    u <- pkgchk_url_from_desc (path)
    out$badges <- list ()
    if (!is.null (u)) {
        out$badges <- pkgchk_ci_badges (u)
        if (!is.null (out$badges)) {
            if (any (grepl ("github", out$badges))) {
                out$github_workflows <- ci_results_gh (path)
            }
        }
    }

    out$gp <- pkgchk_gp_report (path)

    # ----- Add new checks here -----
    # see https://github.com/ropensci-review-tools/pkgcheck/pull/27
    # for an example of how to add new checks
    out$scrap <- pkgchk_has_scrap (path)
    # ----- End add new checks -----

    out$pkg_versions <- version_info (is.null (out$srr))

    class (out) <- c ("pkgcheck", class (out))

    stopfile <- Sys.getenv ("PKGCHECK_PXBG_STOP")
    if (stopfile != "") {
        writeLines ("process stopped", con = stopfile)
    }

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

pkgstats_checks <- function (path) {

    s <- suppressWarnings (pkgstats::pkgstats (path))
    s$path <- path

    out <- list ()
    out$package <- pkgchk_pkg_name (s)
    out$version <- pkgchk_pkg_version (s)
    out$url <- pkgchk_url_from_desc (path)
    out$license <- pkgchk_pkg_license (s)

    out$summary <- pkgchk_pkgstats_summary (s)

    out$git <- pkgchk_git_info (path)

    out$srr <- pkgchk_srr_report (path)

    out$file_list <- list ()
    out$file_list$uses_roxy <- pkgchk_uses_roxygen2 (path)
    out$file_list$has_contrib <- pkgchk_has_contrib_md (path)
    out$file_list$has_citation <- pkgchk_has_citation (path)
    out$file_list$has_codemeta <- pkgchk_has_codemeta (path)
    out$file_list$pkgname_available <- pkgchk_pkgname_available (path)
    out$file_list$pkg_on_cran <- pkgchk_on_cran (path)

    out$fns_have_exs <- pkgchk_pkg_fns_have_exs (path)

    out$left_assigns <- pkgchk_left_assign (path) # tallies of "<-", "<<-", "="

    out$file_list$has_url <- !is.na (s$desc$urls)
    out$file_list$has_bugs <- !is.na (s$desc$bugs)

    out$pkgstats <- fmt_pkgstats_checks (s)

    return (list (stats = s,
                  out = out))
}

#' Format \pkg{pkgstats} data
#' @param s Output of \pkg{pkgstats} call.
#' @return Report as formatted string
#' @noRd
fmt_pkgstats_checks <- function (s) {

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

fn_call_network <- function (s) {

    if (nrow (s$stats$network) == 0L && nrow (s$stats$objects) == 0L)
        return (NULL)

    visjs_dir <- file.path (getOption ("pkgcheck.cache_dir"),
                             "static")
    if (!dir.exists (visjs_dir))
        dir.create (visjs_dir, recursive = TRUE)

    visjs_file <- paste0 (s$out$package,
                          "_pkgstats",
                          substring (s$out$git$HEAD, 1, 8),
                          ".html")
    visjs_path <- file.path (visjs_dir, visjs_file)

    # clean up any older ones
    flist <- list.files (visjs_dir,
                         pattern = paste0 (s$out$package, "_pkgstats"),
                         full.names = TRUE)

    if (!visjs_path %in% flist) {

        unlink (flist, recursive = TRUE)
        pkgstats::plot_network (s$stats, vis_save = visjs_path)
        # visNetwork renames the generic `lib` folder to the specific name, so
        # needs to be cleaned up:
        flist <- list.files (visjs_dir,
                             pattern = paste0 (s$out$package, "_pkgstats"),
                             full.names = TRUE)
        libdir <- flist [which (dir.exists (flist))]
        if (!"lib" %in% list.files (visjs_dir)) {
            if (length (libdir) > 0) {
                libdir <- libdir [1]
                fpath <- file.path (libdir, "..")
                newlibdir <- file.path (normalizePath (fpath), "lib")
                file.rename (libdir, newlibdir)
            }
        } else {
            unlink (libdir)
        }
    }

    return (visjs_path)
}

version_info <- function (nosrr) {

    pkgs <- c ("pkgstats", "pkgcheck")
    if (!nosrr)
        pkgs <- c (pkgs, "srr")

    vapply (pkgs, function (i)
            paste0 (utils::packageVersion (i)),
            character (1))
}

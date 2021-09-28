
#' Generate report on package compliance with rOpenSci Statistical Software
#' requirements
#'
#' @param path Path to local repository
#' @return A `pkgcheck` object detailing all package assessments automatically
#' applied to packages submitted for peer review.
#' @family pkgcheck_fns
#' @export
pkgcheck <- function (path = ".") {

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

    out <- pkgcheck_object ()
    out$package <- s$out [c ("package", "version", "url",
                             "license", "summary", "dependencies")]
    names (out$package) [1] <- "name"

    out$info <- s$out [c ("git", "srr", "pkgstats",
                          "fns_have_exs", "left_assigns")]

    out$checks <- file_checks (path)
    out$checks$has_url <- !is.na (s$stats$desc$urls)
    out$checks$has_bugs <- !is.na (s$stats$desc$bugs)

    out$info$network_file <- fn_call_network (s)

    u <- pkginfo_url_from_desc (path)
    out$info$badges <- list ()
    if (!is.null (u)) {
        out$info$badges <- pkgchk_ci_badges (u)
        if (!is.null (out$info$badges)) {
            if (any (grepl ("github", out$info$badges))) {
                out$info$github_workflows <- ci_results_gh (path)
            }
        }
    }

    out$checks$gp <- pkgchk_gp_report (path)

    out$meta <- version_info (is.null (out$info$srr))

    stopfile <- Sys.getenv ("PKGCHECK_PXBG_STOP")
    if (stopfile != "") {
        writeLines ("process stopped", con = stopfile)
    }

    return (out)
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
    out$version <- pkginfo_pkg_version (s)
    out$url <- pkginfo_url_from_desc (path)
    out$license <- pkginfo_pkg_license (s)

    out$summary <- pkginfo_pkgstats_summary (s)
    out$dependencies <- parse_pkg_deps (s)

    out$git <- pkginfo_git_info (path)

    out$srr <- pkginfo_srr_report (path)

    out$fns_have_exs <- pkgchk_pkg_fns_have_exs (path)

    out$left_assigns <- pkgchk_left_assign (path) # tallies of "<-", "<<-", "="

    out$pkgstats <- fmt_pkgstats_info (s)

    return (list (stats = s,
                  out = out))
}

file_checks <- function (path) {

    res <- list ()
    res$uses_roxy <- pkgchk_uses_roxygen2 (path)
    res$has_contrib <- pkgchk_has_contrib_md (path)
    res$has_citation <- pkgchk_has_citation (path)
    res$has_codemeta <- pkgchk_has_codemeta (path)
    res$vignette <- pkgchk_has_vignette (path)
    res$pkgname_available <- pkgchk_pkgname_available (path)
    res$pkg_on_cran <- pkgchk_on_cran (path)
    res$scrap <- pkgchk_has_scrap (path)

    return (res)
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

    if (nzchar (Sys.getenv("PKGCHECK_TESTING"))) {
        return (
            vapply (pkgs, function (i)
            "42",
            character (1))
        )
    }

    vapply (pkgs, function (i)
            paste0 (utils::packageVersion (i)),
            character (1))
}

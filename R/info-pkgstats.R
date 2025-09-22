pkgstats_info <- function (path, use_cache) {

    s <- suppressWarnings (
        cache_pkgcheck_component (
            path,
            use_cache,
            renv_activated = FALSE,
            what = "pkgstats"
        )
    )
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

    if (s$desc$package != "srr") {
        # 'srr' package has tags, but report fails, so ignore here
        out$srr <- pkginfo_srr_report (path)
    }

    out$pkgstats <- fmt_pkgstats_info (s)

    out$fn_names <- pkgstats::pkgstats_fn_names (path)

    return (list (
        stats = s,
        out = out,
        from_cache = attr (s, "from_cache")
    ))
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


#' @param s Result of `pkgstats(path)`
#' @noRd
pkginfo_pkg_name <- function (s) {

    s$desc$package
}

pkginfo_pkg_version <- function (s) {

    s$desc$version
}

pkginfo_pkg_license <- function (s) {

    s$desc$license
}

pkginfo_pkgstats_summary <- function (s) {

    pkgstats <- fmt_pkgstats_info (s)

    num_exported_fns <- pkgstats$value [pkgstats$measure == "n_fns_r_exported"]
    num_non_exported_fns <- pkgstats$value [pkgstats$measure ==
        "n_fns_r_not_exported"]
    num_src_fns <- sum (pkgstats$value [pkgstats$measure %in%
        c ("n_fns_src", "n_fns_inst")])
    loc_exported_fns <- pkgstats$value [pkgstats$measure ==
        "loc_per_fn_r_exp"]
    loc_non_exported_fns <- pkgstats$value [pkgstats$measure ==
        "loc_per_fn_r_not_exp"]
    loc_src_fns <- stats::median (pkgstats$value [pkgstats$measure %in%
        c ("loc_per_fn_src", "loc_per_fn_inst")])
    num_params_per_fn <- pkgstats$value [pkgstats$measure ==
        "num_params_per_fn"]

    list (
        num_authors = s$desc$aut,
        num_vignettes = unname (s$vignettes [1]),
        num_data = unname (s$data_stats [1]),
        imported_pkgs = length (strsplit (s$desc$imports, ",") [[1]]),
        num_exported_fns = as.integer (num_exported_fns),
        num_non_exported_fns = as.integer (num_non_exported_fns),
        num_src_fns = as.integer (num_src_fns),
        loc_exported_fns = as.integer (loc_exported_fns),
        loc_non_exported_fns = as.integer (loc_non_exported_fns),
        loc_src_fns = as.integer (loc_src_fns),
        num_params_per_fn = as.integer (num_params_per_fn),
        languages = attr (pkgstats, "language")
    )
}

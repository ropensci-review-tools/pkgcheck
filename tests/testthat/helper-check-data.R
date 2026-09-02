requireNamespace ("memoise", quietly = TRUE)

make_check_data_internal <- function (cleanup = TRUE) {

    withr::local_envvar (
        list (
            "PKGCHECK_SRR_REPORT_FILE" = "report.html",
            "PKGCHECK_TEST_NETWORK_FILE" = "network.html",
            "PKGCHECK_CACHE_DIR" = file.path (tempdir (), "pkgcheck"),
            "GITHUB_ACTIONS" = "true",
            "GITHUB_REPOSITORY" = "org/repo",
            "GP_EXCLUDE_CHECK_GROUPS" = "covr,cyclocomp,lintr,rcmdcheck"
        )
    )

    f <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
    path <- pkgstats::extract_tarball (f)
    if (cleanup) {
        on.exit (fs::dir_delete (path), add = TRUE)
    }

    pkgcheck (path, goodpractice = FALSE)
}

make_check_data <- memoise::memoise (make_check_data_internal)

make_check_data_srr_internal <- function (goodpractice = FALSE, cleanup = TRUE) {

    evs <- list (
        "PKGCHECK_SRR_REPORT_FILE" = "report.html",
        "PKGCHECK_TEST_NETWORK_FILE" = "network.html",
        "PKGCHECK_CACHE_DIR" = file.path (tempdir (), "pkgcheck"),
        "GITHUB_ACTIONS" = "true",
        "GITHUB_REPOSITORY" = "org/repo"
    )
    if (!goodpractice) {
        evs$GP_EXCLUDE_CHECK_GROUPS <- "covr,cyclocomp,lintr,rcmdcheck"
    }
    withr::local_envvar (evs)

    pkgname <- paste0 (
        "testpkg", ifelse (goodpractice, "with", "no"), "gp"
    )
    d <- srr::srr_stats_pkg_skeleton (pkg_name = pkgname)
    if (cleanup) {
        on.exit (fs::dir_delete (d), add = TRUE)
    }

    x <- capture.output (
        roxygen2::roxygenise (d, load_code = roxygen2::load_source),
        type = "message"
    )

    pkgcheck (d, goodpractice = goodpractice, use_cache = FALSE)
}

make_check_data_srr <- memoise::memoise (make_check_data_srr_internal)

requireNamespace ("memoise", quietly = TRUE)

make_check_data_internal <- function () {

    withr::local_envvar (
        list (
            "PKGCHECK_SRR_REPORT_FILE" = "report.html",
            "PKGCHECK_TEST_NETWORK_FILE" = "network.html",
            "PKGCHECK_CACHE_DIR" = file.path (tempdir (), "pkgcheck"),
            "GITHUB_ACTIONS" = "true",
            "GITHUB_REPOSITORY" = "org/repo"
        )
    )

    f <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
    path <- pkgstats::extract_tarball (f)
    pkgcheck (path, goodpractice = FALSE)
}

make_check_data <- memoise::memoise (make_check_data_internal)

make_check_data_srr_internal <- function (goodpractice = FALSE) {

    withr::local_envvar (
        list (
            "PKGCHECK_SRR_REPORT_FILE" = "report.html",
            "PKGCHECK_TEST_NETWORK_FILE" = "network.html",
            "PKGCHECK_CACHE_DIR" = file.path (tempdir (), "pkgcheck"),
            "GITHUB_ACTIONS" = "true",
            "GITHUB_REPOSITORY" = "org/repo"
        )
    )

    pkgname <- paste0 (
        "testpkg", ifelse (goodpractice, "with", "no"), "gp"
    )
    d <- srr::srr_stats_pkg_skeleton (pkg_name = pkgname)

    x <- capture.output (
        roxygen2::roxygenise (d, load_code = roxygen2::load_source),
        type = "message"
    )

    checks <- pkgcheck (d, goodpractice = goodpractice, use_cache = FALSE)
    if (goodpractice) {
        class (checks$goodpractice$covr) <- c (
            "try-error",
            class (checks$goodpractice$covr)
        )
        class (checks$goodpractice$rcmdcheck) <- c (
            "try-error",
            class (checks$goodpractice$rcmdcheck)
        )
    }

    return (checks)
}

make_check_data_srr <- memoise::memoise (make_check_data_srr_internal)

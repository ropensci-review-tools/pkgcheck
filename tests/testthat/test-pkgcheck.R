
source ("../clean-snapshots.R")

test_that ("pkgcheck", {

    withr::local_envvar (list ("PKGCHECK_SRR_REPORT_FILE" = "report.html"))
    withr::local_envvar (list ("PKGCHECK_TEST_NETWORK_FILE" = "network.html"))
    withr::local_options (list (
        "pkgcheck.cache_dir" =
            file.path (tempdir (), "pkgcheck")
    ))

    pkgname <- "testpkgchecknotapkg"
    d <- srr::srr_stats_pkg_skeleton (pkg_name = pkgname)

    x <- capture.output (
        roxygen2::roxygenise (d),
        type = "message"
    )

    expect_true (length (x) > 10)
    expect_true (any (grepl ("srrstats", x)))

    expect_output (
        checks <- pkgcheck (d)
    )
    expect_type (checks, "list")

    # goodpractice -> rcmdcheck fails on some machines for reasons that can't be
    # controlled (such as not being able to find "MASS" pkg).
    checks$goodpractice <- NULL
    # Checks on systems without the right API keys may fail checks which rely on
    # URL queries, so these are manually reset here:
    checks$checks$pkgname_available <- TRUE
    checks$info$badges <- NULL # then fails CI checks

    items <- c ("pkg", "info", "checks", "meta")
    expect_true (all (items %in% names (checks)))

    items <- c (
        "name", "path", "version", "url", "BugReports",
        "license", "summary", "dependencies"
    )
    expect_true (all (items %in% names (checks$pkg)))

    items <- c ("git", "srr", "pkgstats", "network_file")
    expect_true (all (items %in% names (checks$info)))

    md <- checks_to_markdown (checks, render = FALSE)

    a <- attributes (md)
    expect_true (length (a) > 0L)
    expect_true (
        all (c (
            "checks_okay",
            "is_noteworthy",
            "network_file",
            "srr_report_file"
        ) %in% names (a))
    )

    # *****************************************************************
    # ***********************   SNAPSHOT TEST   ***********************
    # *****************************************************************

    md <- edit_markdown (md) # from clean-snapshots.R

    md_dir <- withr::local_tempdir ()
    writeLines (md, con = file.path (md_dir, "checks.md"))

    testthat::expect_snapshot_file (file.path (md_dir, "checks.md"))

    h <- render_markdown (md, open = FALSE)
    f <- file.path (md_dir, "checks.html")
    file.rename (h, f)
    edit_html (f) # from clean-snapshots.R

    testthat::expect_snapshot_file (f)
})

test_that ("pkgcheck without goodpractice", {
    pkgname <- paste0 (
        sample (c (letters, LETTERS), 8),
        collapse = ""
    )
    d <- srr::srr_stats_pkg_skeleton (pkg_name = pkgname)

    x <- capture.output (
        roxygen2::roxygenise (d),
        type = "message"
    )

    withr::local_options (list (
        "pkgcheck.cache_dir" =
            file.path (tempdir (), "pkgcheck")
    ))

    expect_output (
        checks <- pkgcheck (d, goodpractice = FALSE)
    )

    # items from above including goodpractice:
    items <- c ("pkg", "info", "checks", "meta", "goodpractice")
    expect_false (all (items %in% names (checks)))
    items <- c ("pkg", "info", "checks", "meta")
    expect_true (all (items %in% names (checks)))
})

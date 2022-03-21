
test_that ("goodpractice", {

    withr::local_envvar (list ("PKGCHECK_SRR_REPORT_FILE" = "report.html"))
    withr::local_envvar (list ("PKGCHECK_TEST_NETWORK_FILE" = "network.html"))
    withr::local_envvar (list (
        "PKGCHECK_CACHE_DIR" =
            file.path (tempdir (), "pkgcheck")
    ))
    withr::local_envvar (list ("GITHUB_ACTIONS" = "true"))
    withr::local_envvar (list ("GITHUB_REPOSITORY" = "org/repo"))

    pkgname <- paste0 (sample (c (letters, LETTERS), 8), collapse = "")
    d <- srr::srr_stats_pkg_skeleton (pkg_name = pkgname)

    x <- capture.output (
        roxygen2::roxygenise (d),
        type = "message"
    )

    expect_output (
        checks <- pkgcheck (d)
    )

    gp <- summarise_gp_checks (checks)
    expect_type (gp, "list")
    expect_length (gp, 2L)
    expect_identical (names (gp), c ("rcmd_errs", "rcmd_warns"))

    md <- gp_checks_to_md (checks)
    expect_type (md, "character")
    expect_true (length (md) > 10L)
    expect_true (any (grepl ("`goodpractice` results", md)))
    expect_true (any (grepl ("R CMD check", md)))
    expect_true (any (grepl ("Test Coverage", md)))
    expect_true (any (grepl ("Static code analyses", md)))

    checks$goodpractice$rcmdcheck <- try (stop ("nope"), silent = TRUE)
    gp <- summarise_gp_checks (checks)
    expect_null (gp$rcmd_warns)
    expect_true (grepl ("R CMD check process failed", gp$rcmd_errs))
})

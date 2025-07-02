test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") ||
    identical (Sys.getenv ("GITHUB_JOB"), "test-coverage") ||
    identical (Sys.getenv ("GITHUB_JOB"), "pkgcheck"))

skip_if (!test_all)

test_that ("goodpractice", {

    checks <- make_check_data_srr (goodpractice = TRUE)

    gp <- summarise_gp_checks (checks)
    expect_type (gp, "list")
    expect_length (gp, 2L)
    expect_named (gp, c ("rcmd_errs", "rcmd_warns"))

    md <- gp_checks_to_md (checks)
    expect_type (md, "character")
    expect_gt (length (md), 10L)
    expect_true (any (grepl ("`goodpractice` results", md)))
    expect_true (any (grepl ("R CMD check", md)))
    # expect_true (any (grepl ("Test Coverage", md)))
    expect_true (any (grepl ("Cyclocomplexity", md)))

    checks$goodpractice$rcmdcheck <- try (stop ("nope"), silent = TRUE)
    gp <- summarise_gp_checks (checks)
    expect_null (gp$rcmd_warns)
    expect_true (grepl ("R CMD check process failed", gp$rcmd_errs))
})

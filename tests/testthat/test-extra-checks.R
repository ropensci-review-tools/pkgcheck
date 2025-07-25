test_all <- identical (Sys.getenv ("MPADGE_LOCAL"), "true") ||
    identical (Sys.getenv ("GITHUB_JOB"), "test-coverage")

# These tests should not be skipped because the `!test_all` condition then
# includes the `pkgcheck` workflow itself, which then reduces coverage.
# skip_if (!test_all)
testthat::skip_on_os ("windows")
testthat::skip_on_os ("mac")

test_that ("extra checks", {

    checks <- make_check_data ()

    # Checks on systems without the right API keys may fail checks which rely on
    # URL queries, so these are manually reset here:
    checks$checks$pkgname_available <- TRUE
    checks$info$badges <- character (0)

    # Then fake the extra checks for the output methods:
    checks$checks$has_scrap <- c ("a", "b")
    checks$checks$obsolete_pkg_deps <- c ("blah", "sp", "rgdal")
    checks$info$srr <- list (
        message = "srr message",
        okay = TRUE
    )
    checks$checks$srr_okay <- TRUE

    withr::local_envvar (
        list (
            "PKGCHECK_CACHE_DIR" = file.path (tempdir (), "pkgcheck"),
            "GITHUB_ACTIONS" = "true"
        )
    )

    md <- checks_to_markdown (checks)

    # *****************************************************************
    # ***********************   SNAPSHOT TEST   ***********************
    # *****************************************************************
    #
    md <- edit_markdown (md) # from clean-snapshots.R

    md_dir <- withr::local_tempdir ()
    f_md <- fs::path (md_dir, "checks-extra.md")
    writeLines (md, con = f_md)

    testthat::expect_snapshot_file (file.path (md_dir, "checks-extra.md"))

    h <- render_md2html (md, open = FALSE)
    f_html <- file.path (md_dir, "checks-extra.html")
    file.copy (h, f_html)
    edit_html (f_html) # from clean-snapshots.R

    testthat::expect_snapshot_file (f_html)

    # Then snapshot tests of print & summary methods
    # This loads goodpractice, so first do that to avoid load message
    requireNamespace ("goodpractice")
    f_tmp <- tempfile (fileext = ".md")
    set.seed (1L) # for praise.R random sample output
    x <- capture.output (print (checks), file = f_tmp, type = "message")

    md <- edit_markdown (readLines (f_tmp), print_method = TRUE)
    md_dir <- withr::local_tempdir ()
    f_md2 <- fs::path (md_dir, "checks-print.md")
    writeLines (md, con = f_md2)

    testthat::expect_snapshot_file (file.path (md_dir, "checks-print.md"))

    fs::file_delete (c (f_md, f_html, f_tmp, f_md2))

    # *****************************************************************
    # *********   EXTRA CHECKS FOR PRAISE IN SUMMARY OUTPUT   *********
    # *****************************************************************
    set.seed (1L)
    x <- capture.output (summary (checks), type = "message")
    expect_true (any (grepl ("Sorry about that", x)))
    withr::with_envvar (
        list ("ROPENSCI_REVIEW_BOT" = "true"),
        x <- capture.output (summary (checks), type = "message")
    )
    expect_false (any (grepl ("Sorry about that", x)))
    expect_true (any (grepl ("This package is not ready to be submitted", x)))

    # Fix up all failing checks:
    checks$checks$has_contrib_md <- checks$checks$has_codemeta <-
        checks$checks$has_vignette <- TRUE
    checks$checks$fns_have_return_vals <- checks$checks$has_scrap <- NULL
    ex <- checks$checks$fns_have_exs
    ex [which (!ex)] <- TRUE
    checks$checks$fns_have_exs <- ex
    checks$checks$branch_is_master <- FALSE

    set.seed (1L)
    x <- capture.output (summary (checks), type = "message")
    expect_true (any (grepl ("great work", x)))
    withr::with_envvar (
        list ("ROPENSCI_REVIEW_BOT" = "true"),
        x <- capture.output (summary (checks), type = "message")
    )
    expect_false (any (grepl ("great work", x)))
    expect_true (any (grepl ("This package may be submitted", x)))
})

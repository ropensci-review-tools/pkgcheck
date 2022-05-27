
source ("../clean-snapshots.R")

# These tests fail on GHA on both mac and windows because they can't find the
# tarball.
skip_on_os ("mac")
skip_on_os ("windows")

test_that ("extra checks", {

    withr::local_envvar (list ("PKGCHECK_SRR_REPORT_FILE" = "report.html"))
    withr::local_envvar (list ("PKGCHECK_TEST_NETWORK_FILE" = "network.html"))
    withr::local_envvar (list (
        "PKGCHECK_CACHE_DIR" =
            file.path (tempdir (), "pkgcheck")
    ))
    withr::local_envvar (list ("GITHUB_ACTIONS" = "true"))
    withr::local_envvar (list ("GITHUB_REPOSITORY" = "org/repo"))

    f <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
    path <- pkgstats::extract_tarball (f)
    checks <- pkgcheck (path, goodpractice = FALSE)
    # Checks on systems without the right API keys may fail checks which rely on
    # URL queries, so these are manually reset here:
    checks$checks$pkgname_available <- TRUE
    checks$info$badges <- character (0)

    # Then fake the extra checks for the output methods:
    checks$checks$has_scrap <- c ("a", "b")
    checks$checks$obsolete_pkg_deps <- c ("blah", "sp", "rgdal")

    md <- checks_to_markdown (checks)

    # *****************************************************************
    # ***********************   SNAPSHOT TEST   ***********************
    # *****************************************************************
    #
    md <- edit_markdown (md) # from clean-snapshots.R

    md_dir <- withr::local_tempdir ()
    writeLines (md, con = file.path (md_dir, "checks-extra.md"))

    testthat::expect_snapshot_file (file.path (md_dir, "checks-extra.md"))

    h <- render_markdown (md, open = FALSE)
    f <- file.path (md_dir, "checks-extra.html")
    file.copy (h, f)
    edit_html (f) # from clean-snapshots.R

    testthat::expect_snapshot_file (f)

    # Then snapshot tests of print & summary methods
    # This loads goodpractice, so first do that to avoid load message
    requireNamespace ("goodpractice")
    f <- tempfile (fileext = ".md")
    x <- capture.output (print (checks), file = f, type = "message")

    md <- edit_markdown (readLines (f), print_method = TRUE)
    md_dir <- withr::local_tempdir ()
    writeLines (md, con = file.path (md_dir, "checks-print.md"))

    testthat::expect_snapshot_file (file.path (md_dir, "checks-print.md"))
})

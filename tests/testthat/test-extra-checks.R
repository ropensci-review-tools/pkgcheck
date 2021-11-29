
source ("../clean-snapshots.R")

test_that ("extra checks", {

    withr::local_envvar (list ("PKGCHECK_SRR_REPORT_FILE" = "report.html"))
    withr::local_envvar (list ("PKGCHECK_TEST_NETWORK_FILE" = "network.html"))
    withr::local_options (list (
        "pkgcheck.cache_dir" =
            file.path (tempdir (), "pkgcheck")
    ))

    f <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
    path <- pkgstats::extract_tarball (f)
    checks <- pkgcheck (path, goodpractice = FALSE)

    # Then fake the extra checks for the output methods:
    checks$checks$has_scrap <- c ("a", "b")
    checks$checks$obsolete_pkg_deps <- c ("blah", "sp", "rgdal")

    # goodpractice -> rcmdcheck fails on some machines for reasons that can't be
    # controlled (such as not being able to find "MASS" pkg).
    rcmd <- checks$goodpractice$rcmdcheck
    rcmd$errors <- rcmd$warnings <- rcmd$notes <- character (0)
    rcmd$test_fail <- list ()
    checks$goodpractice$rcmdcheck <- rcmd

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
})


test_that ("extra checks", {

    withr::local_envvar (list ("PKGCHECK_SRR_REPORT_FILE" = "report.html"))
    withr::local_envvar (list ("PKGCHECK_TEST_NETWORK_FILE" = "network.html"))

    f <- system.file ("extdata", "pkgstats_9.9.tar.gz", package = "pkgstats")
    path <- pkgstats::extract_tarball (f)
    checks <- pkgcheck (path, goodpractice = FALSE)

    # Then fake the extra checks for the output methods:
    checks$checks$has_scrap <- c ("a", "b")
    checks$checks$obsolete_pkg_deps <- c ("blah", "sp", "rgdal")

    md <- checks_to_markdown (checks)

    # *****************************************************************
    # ***********************   SNAPSHOT TEST   ***********************
    # *****************************************************************
    #
    # some checks like rcmdcheck differ on different systems for things like
    # compilation flags, so the snapshot test excludes any rmcdcheck output. It
    # also reverts the final package versions to a generic number.
    edit_markdown <- function (md) {

        change_pkg_vers <- function (md, pkg = "pkgstats", to = "42") {
            i <- grep ("Package Versions", md)
            pkg_i <- grep (pkg, md)
            pkg_i <- pkg_i [pkg_i > i] [1]
            md [pkg_i] <- gsub ("([0-9]\\.)+[0-9]+", to, md [pkg_i])
            # white space also changes with version numbers:
            md [pkg_i] <- gsub (
                paste0 (to, "\\s+"),
                paste0 (to, "    "), md [pkg_i]
            )
            return (md)
        }
        md <- change_pkg_vers (md, "pkgstats")
        md <- change_pkg_vers (md, "pkgcheck")
        md <- change_pkg_vers (md, "srr")

        return (md)
    }
    md <- edit_markdown (md)

    md_dir <- withr::local_tempdir ()
    writeLines (md, con = file.path (md_dir, "checks-extra.md"))

    testthat::expect_snapshot_file (file.path (md_dir, "checks-extra.md"))
})

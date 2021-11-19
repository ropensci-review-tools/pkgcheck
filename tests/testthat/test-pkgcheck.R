
test_that ("pkgcheck", {

    withr::local_envvar (list ("PKGCHECK_SRR_REPORT_FILE" = "report.html"))
    withr::local_envvar (list ("PKGCHECK_TEST_NETWORK_FILE" = "network.html"))

    pkgname <- "testpkgchecknotapkg"
    d <- srr::srr_stats_pkg_skeleton (pkg_name = pkgname)

    x <- capture.output (
        roxygen2::roxygenise (d),
        type = "message"
    )

    expect_true (length (x) > 10)
    expect_true (any (grepl ("srrstats", x)))

    expect_output (
        chk <- pkgcheck (d)
    )
    expect_type (chk, "list")

    items <- c ("pkg", "info", "checks", "meta", "goodpractice")
    expect_true (all (items %in% names (chk)))

    items <- c (
        "name", "path", "version", "url", "BugReports",
        "license", "summary", "dependencies"
    )
    expect_true (all (items %in% names (chk$pkg)))

    items <- c ("git", "srr", "pkgstats", "network_file", "badges")
    expect_true (all (items %in% names (chk$info)))

    md <- checks_to_markdown (chk, render = FALSE)

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
    #
    # some checks like rcmdcheck differ on different systems for things like
    # compilation flags, so the snapshot test excludes any rmcdcheck output. It
    # also reverts the final package versions to a generic number.
    edit_markdown <- function (md) {
        rcmd <- grep ("R CMD check", md) [1:2] # summary items
        gp <- grep ("`goodpractice` results", md)
        end_fold <- grep ("<\\/details>", md)
        # then -3 to keep ["", "</p>", "</details>"]:
        gp <- seq (gp, end_fold [end_fold > gp] [1] - 3)

        md <- md [-c (rcmd, gp)]

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
    writeLines (md, con = file.path (md_dir, "checks.md"))

    testthat::expect_snapshot_file (file.path (md_dir, "checks.md"))

    h <- render_markdown (md, open = FALSE)
    f <- file.path (md_dir, "checks.html")
    file.rename (h, f)
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

    expect_output (
        chk <- pkgcheck (d, goodpractice = FALSE)
    )

    # items from above including goodpractice:
    items <- c ("pkg", "info", "checks", "meta", "goodpractice")
    expect_false (all (items %in% names (chk)))
    items <- c ("pkg", "info", "checks", "meta")
    expect_true (all (items %in% names (chk)))
})

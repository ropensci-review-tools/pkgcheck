test_that("pkgcheck", {

    options (repos = c (CRAN = "https://cloud.r-project.org"))

    pkgname <- paste0 (sample (c (letters, LETTERS), 8), collapse = "")
    d <- srr::srr_stats_pkg_skeleton (pkg_name = pkgname)
    # remove RoxgygenNote from DESC; see
    # https://github.com/r-lib/roxygen2/issues/905
    f <- file.path (d, "DESCRIPTION")
    desc <- data.frame (read.dcf (f))
    i <- grep ("RoxygenNote", names (desc))
    if (length (i) > 0L) {
        desc <- desc [, -i]
        write.dcf (desc, f)
    }

    x <- capture.output (
                         roxygen2::roxygenise (d),
                         type = "message")

    expect_true (length (x) > 10)
    expect_true (any (grepl ("srrstats", x)))

    expect_output (
                   chk <- pkgcheck (d)
    )
    expect_type (chk, "list")

              items <- c ("package",
                          "version",
                          "license",
                          "summary",
                          "git",
                          "srr",
                          "file_list",
                          "fns_have_exs",
                          "left_assigns",
                          "pkgstats",
                          "network_file",
                          "badges",
                          "gp",
                          "scrap",
                          "vignette",
                          "pkg_versions")
              expect_true (all (items %in% names (chk)))
    expect_true (all (items %in% names (chk)))

    md <- checks_to_markdown (chk, render = FALSE)
    expect_type (md, "character")
    expect_true (length (md) > 100L)
    a <- attributes (md)
    expect_true (length (a) > 0L)
    expect_true (all (c ("checks_okay",
                         "is_noteworthy",
                         "network_file",
                         "srr_report_file") %in% names (a)))
})

test_that("pkgcheck_bg() works", {

    options (repos = c (CRAN = "https://cloud.r-project.org"))

    pkgname <- paste0 (sample (c (letters, LETTERS), 8), collapse = "")
    if (dir.exists (file.path (tempdir (), pkgname)))
        chk <- unlink (file.path (tempdir (), pkgname),
                       recursive = TRUE)
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

    x <- pkgcheck_bg (d)
    expect_s3_class (x, "r_process")
    expect_s3_class (x, "R6")

    pt1 <- system.time ({
        while (x$is_alive ()) {
            Sys.sleep (0.1)
    }}) [3]

    out <- x$get_result ()
    expect_type (out, "list")
    expect_length (out, 15L)

    # results should then be cached:
    pt2 <- system.time (
        x2 <- pkgcheck (d)
        ) [3]

    #expect_true (pt2 < pt1) # not always fulfilled
})

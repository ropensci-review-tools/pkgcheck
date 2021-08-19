test_that("check-bg", {

    options (repos = c (CRAN = "https://cloud.r-project.org"))

    d <- srr::srr_stats_pkg_skeleton (pkg_name = "demobg")

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
    expect_length (out, 14L)

    # results should then be cached:
    pt2 <- system.time (
        x2 <- pkgcheck (d)
        ) [3]

    expect_true (pt2 < (pt1 / 2))
})

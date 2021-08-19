test_that("pkgcheck", {

              options (repos = c (CRAN = "https://cloud.r-project.org"))

              d <- srr::srr_stats_pkg_skeleton ()

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
                          "pkg_versions")
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

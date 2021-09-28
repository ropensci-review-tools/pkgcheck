test_that("pkgcheck", {
  withr::local_envvar(list("PKGCHECK_SRR_REPORT_FILE" = "report.html"))
  withr::local_envvar(list("PKGCHECK_TEST_NETWORK_FILE" = "network.html"))
  withr::local_envvar(list("PKGCHECK_TESTING" = "yep"))

  options (repos = c (CRAN = "https://cloud.r-project.org"))

  pkgname <- "testpkgchecknotapkg"
  d <- srr::srr_stats_pkg_skeleton (pkg_name = pkgname)

  x <- capture.output (
    roxygen2::roxygenise (d),
    type = "message")

  expect_true (length (x) > 10)
  expect_true (any (grepl ("srrstats", x)))

  expect_output (
    chk <- pkgcheck (d)
  )
  expect_type (chk, "list")

  items <- c ("package", "info", "checks", "meta")
  expect_true (all (items %in% names (chk)))

  items <- c ("name", "version", "license", "summary", "dependencies")
  expect_true (all (items %in% names (chk$package)))

  items <- c ("git", "srr", "pkgstats", "fns_have_exs",
              "left_assigns", "network_file", "badges")
  expect_true (all (items %in% names (chk$info)))

  md <- checks_to_markdown (chk, render = FALSE)
  md_dir <- withr::local_tempdir()
  writeLines(md, con = file.path(md_dir, "checks.md"))

  testthat::expect_snapshot_file(file.path(md_dir, "checks.md"))

  a <- attributes (md)
  expect_true (length (a) > 0L)
  expect_true (all (c ("checks_okay",
    "is_noteworthy",
    "network_file",
    "srr_report_file") %in% names (a)))
})

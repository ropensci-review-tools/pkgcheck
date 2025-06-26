test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") ||
    identical (Sys.getenv ("GITHUB_JOB"), "test-coverage") ||
    identical (Sys.getenv ("GITHUB_JOB"), "pkgcheck"))

skip_if (!test_all)

test_that ("pkgcheck", {

    checks0 <- make_check_data_srr (goodpractice = FALSE)
    checks1 <- make_check_data_srr (goodpractice = TRUE)

    expect_s3_class (checks0, "pkgcheck")
    expect_s3_class (checks1, "pkgcheck")

    items0 <- c ("pkg", "info", "checks", "meta")
    expect_named (checks0, items0)
    items1 <- c ("pkg", "info", "checks", "meta", "goodpractice")
    expect_named (checks1, items1)

    items <- c (
        "name", "path", "version", "url", "BugReports",
        "license", "summary", "dependencies", "external_calls",
        "external_fns"
    )
    expect_named (checks0$pkg, items)
    expect_named (checks1$pkg, items)

    items <- c (
        "badges",
        "fn_names",
        "git",
        "network_file",
        "pkgdown_concepts",
        "pkgstats",
        "renv_activated",
        "srr"
    )
    expect_identical (sort (names (checks0$info)), sort (items))
    expect_identical (sort (names (checks1$info)), sort (items))

    md0 <- checks_to_markdown (checks0, render = FALSE)
    md1 <- checks_to_markdown (checks1, render = FALSE)

    a0 <- attributes (md0)
    a1 <- attributes (md1)
    expect_length (a0, 4L)
    expect_length (a1, 4L)
    nms <- c ("checks_okay", "is_noteworthy", "network_file", "srr_report_file")
    expect_true (all (nms %in% names (a0)))
    expect_true (all (nms %in% names (a1)))

    # *****************************************************************
    # ***********************   SNAPSHOT TEST   ***********************
    # *****************************************************************

    # paths in these snapshots are not stable on windows, so skipped here
    skip_on_os ("windows")

    md0 <- edit_markdown (md0) # from clean-snapshots.R
    md1 <- edit_markdown (md1) # from clean-snapshots.R

    md_dir <- withr::local_tempdir ()
    f_md0 <- file.path (md_dir, "checks0.md")
    writeLines (md0, con = f_md0)
    f_md1 <- file.path (md_dir, "checks1.md")
    writeLines (md1, con = f_md1)

    # Redact out variable git hashes:
    testthat::expect_snapshot_file (f_md0)
    testthat::expect_snapshot_file (f_md1)

    h0 <- render_md2html (md0, open = FALSE)
    f_html0 <- file.path (md_dir, "checks0.html")
    file.rename (h0, f_html0)
    edit_html (f_html0) # from clean-snapshots.R

    h1 <- render_md2html (md1, open = FALSE)
    f_html1 <- file.path (md_dir, "checks1.html")
    file.rename (h1, f_html1)
    edit_html (f_html1) # from clean-snapshots.R

    testthat::expect_snapshot_file (f_html0)
    testthat::expect_snapshot_file (f_html1)

    fs::file_delete (c (f_md0, f_md1, f_html0, f_html1))
})

test_all <- (identical (Sys.getenv ("MPADGE_LOCAL"), "true") ||
    identical (Sys.getenv ("GITHUB_JOB"), "test-coverage") ||
    identical (Sys.getenv ("GITHUB_JOB"), "pkgcheck"))

test_that ("check scrap", {

    checks <- make_check_data ()

    ci_out <- output_pkgchk_has_scrap (checks)

    expect_true (ci_out$check_pass)
    expect_length (ci_out$summary, 1L)
    expect_false (nzchar (ci_out$summary))
    expect_length (ci_out$print, 1L)
    expect_false (nzchar (ci_out$print))

    checks$checks$has_scrap <- "scrap"
    ci_out <- output_pkgchk_has_scrap (checks)
    expect_false (ci_out$check_pass)
    expect_equal (
        ci_out$summary,
        "Package contains unexpected files."
    )
    expect_length (ci_out$print, 3L)
    expect_named (ci_out$print, c ("msg_pre", "obj", "msg_post"))
    expect_true ("scrap" %in% ci_out$print$obj)

    skip_if (!test_all)

    # This function needs a fresh repo:
    pkgname <- paste0 (sample (c (letters, LETTERS), 8), collapse = "")
    path <- srr::srr_stats_pkg_skeleton (pkg_name = pkgname)
    withr::with_dir (path, {
        roxygen2::roxygenise (load_code = roxygen2::load_source)
        gert::git_init ()
        gert::git_config_set ("user.name", "Your Name")
        gert::git_config_set ("user.email", "your@email.com")
        flist <- fs::dir_ls () # includes compiled '.o' objects!
        gert::git_add (flist)
        gert::git_commit ("initial commit")
        f <- ".DS_Store"
        writeLines ("", f)
        gert::git_add (f)
        gert::git_commit ("add .DS_Store")
    })
    checks <- pkgcheck::pkgcheck (path, goodpractice = FALSE)
    res <- pkgchk_has_scrap (checks)
    expect_gt (length (res), 0L)
    expect_true (f %in% res)
})

test_that ("check num imports", {

    checks <- make_check_data ()

    ci_out <- output_pkgchk_renv_activated (checks)

    expect_true (ci_out$check_pass)
    expect_length (ci_out$summary, 1L)
    expect_false (nzchar (ci_out$summary))
    expect_length (ci_out$print, 1L)
    expect_false (nzchar (ci_out$print))

    checks$checks$renv_activated <- TRUE
    ci_out <- output_pkgchk_renv_activated (checks)
    expect_false (ci_out$check_pass)
    expect_equal (ci_out$summary, "Package has renv activated")
})

test_that ("info-renv", {

    path <- srr::srr_stats_pkg_skeleton (pkg_name = "junk")
    checks <- list (pkg = list (path = path))

    f <- fs::path (path, "renv.lock")
    writeLines ("", f)
    expect_false (pkginfo_renv_activated (path))

    f <- fs::path (path, ".Rprofile")
    writeLines ("", f)
    expect_false (pkginfo_renv_activated (path))

    writeLines ("source(\"renv/activate.R\")", f)
    expect_true (pkginfo_renv_activated (path))

    renv_deactivate (path)
    expect_false (pkginfo_renv_activated (path))
    # When .Rprofile only has renv, whole file is removed:
    expect_false (fs::file_exists (f))

    writeLines (
        c ("source(\"renv/activate.R\")", "options(width = 80)"),
        f
    )
    expect_true (pkginfo_renv_activated (path))
    renv_deactivate (path)
    # Now file will remain because it has more than just renv line:
    expect_true (fs::file_exists (f))

    fs::dir_delete (path)
})

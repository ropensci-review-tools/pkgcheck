test_that ("check num imports", {

    checks <- make_check_data ()
    checks$checks$on_cran <- FALSE
    checks$checks$pkgname_available <- TRUE

    ci_out <- output_pkgchk_pkgname (checks)

    expect_length (ci_out, 3L)
    expect_named (ci_out, c ("check_pass", "summary", "print"))
    expect_true (ci_out$check_pass)
    expect_length (ci_out$summary, 1L)
    expect_equal (
        ci_out$summary,
        "Package name is available"
    )
    expect_length (ci_out$print, 1L)
    expect_false (nzchar (ci_out$print))

    checks$checks$pkgname_available <- FALSE
    ci_out <- output_pkgchk_pkgname (checks)
    expect_length (ci_out, 3L)
    expect_named (ci_out, c ("check_pass", "summary", "print"))
    expect_false (ci_out$check_pass)
    expect_equal (
        ci_out$summary,
        "Package name is not available (on CRAN)."
    )

    checks$checks$on_cran <- as.logical (NA)
    ci_out <- output_pkgchk_pkgname (checks)
    expect_length (ci_out, 4L)
    expect_named (ci_out, c ("check_pass", "summary", "print", "check_type"))
    expect_equal (ci_out$check_type, "pass_watch")
    expect_equal (
        ci_out$summary,
        "Could not check package name on CRAN"
    )
})

test_that ("check num imports", {

    checks <- make_check_data ()

    ci_out <- output_pkgchk_pkgname (checks)

    expect_true (ci_out$check_pass)
    expect_length (ci_out$summary, 1L)
    # expect_equal (
    #     ci_out$summary,
    #     "Package name is available"
    # )
    expect_length (ci_out$print, 1L)
    expect_false (nzchar (ci_out$print))

    checks$checks$pkgname_available <- FALSE
    ci_out <- output_pkgchk_pkgname (checks)
    expect_false (ci_out$check_pass)
    expect_equal (
        ci_out$summary,
        "Package name is not available (on CRAN)."
    )
})

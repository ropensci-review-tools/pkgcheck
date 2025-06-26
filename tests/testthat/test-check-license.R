test_that ("check default branch", {

    checks <- make_check_data ()

    ci_out <- output_pkgchk_license (checks)

    expect_true (ci_out$check_pass)
    expect_length (ci_out$summary, 1L)
    expect_false (nzchar (ci_out$summary))
    expect_length (ci_out$print, 1L)
    expect_false (nzchar (ci_out$print))

    checks$checks$license <- "nope"
    ci_out <- output_pkgchk_license (checks)
    expect_false (ci_out$check_pass)
    expect_equal (
        ci_out$summary,
        "Package contains unacceptable 'License' entries: [nope]"
    )
})

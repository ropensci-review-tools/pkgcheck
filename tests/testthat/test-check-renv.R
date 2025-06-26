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

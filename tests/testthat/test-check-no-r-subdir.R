test_that ("check subdir in R dir", {

    checks <- make_check_data ()

    ci_out <- output_pkgchk_no_r_subdir (checks)
    expect_type (ci_out, "list")
    expect_length (ci_out, 3L)
    expect_named (ci_out, c ("check_pass", "summary", "print"))

    expect_true (ci_out$check_pass)
    expect_length (ci_out$summary, 1L)
    expect_false (nzchar (ci_out$summary))
    expect_length (ci_out$print, 1L)
    expect_false (nzchar (ci_out$print))

    checks$checks$no_r_subdir <- FALSE
    ci_out <- output_pkgchk_no_r_subdir (checks)
    expect_false (ci_out$check_pass)
    expect_equal (
        ci_out$summary,
        "The R directory includes sub-directories"
    )
    expect_false (nzchar (ci_out$print))
})

test_that ("check left and global assign", {

    checks <- make_check_data ()

    ci_out <- output_pkgchk_global_assign (checks)
    expect_type (ci_out, "list")
    expect_length (ci_out, 3L)
    expect_named (ci_out, c ("check_pass", "summary", "print"))

    expect_true (ci_out$check_pass)
    expect_length (ci_out$summary, 1L)
    expect_true (!nzchar (ci_out$summary))
    expect_length (ci_out$print, 1L)
    expect_true (!nzchar (ci_out$print))

    ci_out <- output_pkgchk_left_assign (checks)
    expect_type (ci_out, "list")
    expect_length (ci_out, 3L)
    expect_named (ci_out, c ("check_pass", "summary", "print"))

    expect_true (ci_out$check_pass)
    expect_length (ci_out$summary, 1L)
    expect_true (!nzchar (ci_out$summary))
    expect_length (ci_out$print, 1L)
    expect_true (!nzchar (ci_out$print))
})

test_that ("check num imports", {

    checks <- make_check_data ()

    ci_out <- output_pkgchk_num_imports (checks)
    expect_type (ci_out, "list")
    expect_length (ci_out, 4L)
    expect_named (ci_out, c ("check_pass", "summary", "print", "check_type"))

    expect_true (ci_out$check_pass)
    expect_length (ci_out$summary, 1L)
    expect_false (nzchar (ci_out$summary))
    expect_length (ci_out$print, 1L)
    expect_false (nzchar (ci_out$print))
    expect_equal (ci_out$check_type, "none_watch")

    # 2nd val is percentile
    checks$checks$num_imports [2] <- 0.99
    ci_out <- output_pkgchk_num_imports (checks)
    expect_false (ci_out$check_pass)
    expect_equal (
        ci_out$summary,
        "Package has unusually large number of 9 Imports (> 99% of all packages)"
    )
})

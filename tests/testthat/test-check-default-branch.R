test_that ("check default branch", {

    checks <- make_check_data ()

    ci_out <- output_pkgchk_branch_is_master (checks)
    expect_type (ci_out, "list")
    expect_length (ci_out, 3L)
    expect_named (ci_out, c ("check_pass", "summary", "print"))

    expect_false (ci_out$check_pass)
    expect_equal (
        ci_out$summary,
        "Default GitHub branch of 'master' is not acceptable."
    )
    expect_length (ci_out$print, 1L)
    expect_false (nzchar (ci_out$print))

    # This makes no difference:
    checks$info$git <- list (branch = "master")
    ci_out2 <- output_pkgchk_branch_is_master (checks)
    expect_identical (ci_out, ci_out2)
})

test_that ("check repo is not a fork", {

    checks <- make_check_data ()

    ci_out <- output_pkgchk_repo_not_fork (checks)
    expect_type (ci_out, "list")
    expect_length (ci_out, 3L)
    expect_named (ci_out, c ("check_pass", "summary", "print"))

    expect_true (ci_out$check_pass)
    expect_length (ci_out$summary, 1L)
    expect_false (nzchar (ci_out$summary))
    expect_length (ci_out$print, 1L)
    expect_false (nzchar (ci_out$print))

    checks$checks$repo_not_fork <- FALSE
    ci_out <- output_pkgchk_repo_not_fork (checks)
    expect_false (ci_out$check_pass)
    expect_equal (
        ci_out$summary,
        "Repository is a fork, not an original source repository"
    )
    expect_false (nzchar (ci_out$print))
})

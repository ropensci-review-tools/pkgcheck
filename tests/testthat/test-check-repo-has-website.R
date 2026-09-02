test_that ("check repo has a website", {

    checks <- make_check_data ()

    ci_out <- output_pkgchk_repo_has_website (checks)
    expect_type (ci_out, "list")
    expect_length (ci_out, 3L)
    expect_named (ci_out, c ("check_pass", "summary", "print"))

    expect_true (ci_out$check_pass)
    expect_length (ci_out$summary, 1L)
    expect_equal (ci_out$summary, "Repository has a website")
    expect_length (ci_out$print, 1L)
    expect_false (nzchar (ci_out$print))

    checks$checks$repo_has_website <- FALSE
    ci_out <- output_pkgchk_repo_has_website (checks)
    expect_false (ci_out$check_pass)
    expect_equal (ci_out$summary, "Repository has no website")
    expect_false (nzchar (ci_out$print))
})

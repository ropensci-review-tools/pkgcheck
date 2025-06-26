test_that ("check ci", {

    checks <- make_check_data ()

    ci_out <- output_pkgchk_ci (checks)
    expect_type (ci_out, "list")
    expect_length (ci_out, 3L)
    expect_named (ci_out, c ("check_pass", "summary", "print"))

    expect_false (ci_out$check_pass)
    expect_equal (
        ci_out$summary,
        " Package has no continuous integration checks."
    )
    expect_length (ci_out$print, 1L)
    expect_true (!nzchar (ci_out$print))

    checks$checks$has_url <- FALSE
    ci_out <- output_pkgchk_ci (checks)
    expect_false (ci_out$check_pass)
    expect_equal (
        ci_out$summary,
        paste0 (
            "Continuous integration checks unavailable ",
            "(no URL in 'DESCRIPTION')."
        )
    )
    expect_length (ci_out$print, 1L)
    expect_true (!nzchar (ci_out$print))
    checks$checks$has_url <- TRUE


    # workflow has 1 workflow named "Update pkgstats Results"
    checks$info$github_workflows$name <- "R CMD check"
    ci_out <- output_pkgchk_ci (checks)
    expect_true (ci_out$check_pass)
    expect_equal (
        ci_out$summary,
        " Package has continuous integration checks."
    )
    expect_gt (length (ci_out$print), 1L)

    checks$info$github_workflows$conclusion <- "fail"
    ci_out <- output_pkgchk_ci (checks)
    expect_false (ci_out$check_pass)
    expect_equal (
        ci_out$summary,
        " Package fails continuous integration checks."
    )
    expect_length (ci_out$print, 1L)
    expect_true (!nzchar (ci_out$print))

    checks$info$badges <- character (0L)
    ci_out <- output_pkgchk_ci (checks)
    expect_false (ci_out$check_pass)
    expect_equal (
        ci_out$summary,
        paste0 (
            " Package fails continuous integration checks, ",
            "and has no badges on README"
        )
    )
    expect_length (ci_out$print, 1L)
    expect_true (!nzchar (ci_out$print))

    checks$info$github_workflows$conclusion <- "success"
    ci_out <- output_pkgchk_ci (checks)
    expect_true (ci_out$check_pass)
    expect_equal (
        ci_out$summary,
        paste0 (
            " Package has continuous integration checks, ",
            "but no badges on README"
        )
    )
    expect_gt (length (ci_out$print), 0L)
    # Messge in print when no badges detected:
    expect_true (any (grepl ("There do not appear to be any", ci_out$print)))
})

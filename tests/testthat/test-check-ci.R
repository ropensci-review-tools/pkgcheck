test_that ("check ci", {

    checks <- make_check_data ()

    ci_out <- output_pkgchk_ci (checks)
    expect_type (ci_out, "list")
    expect_length (ci_out, 3L)
    expect_named (ci_out, c ("check_pass", "summary", "print"))

    expect_true (ci_out$check_pass)
    expect_equal (
        ci_out$summary,
        " Package has continuous integration checks."
    )
    expect_gt (length (ci_out$print), 5L)
    # Most 'print' lines have something:
    nlines <- length (ci_out$print)
    non_empty_lines <- length (which (nzchar (ci_out$print)))
    expect_gt (non_empty_lines / nlines, 0.6)

    skip_on_os ("mac")

    i <- grep ("CMD", checks$info$github_workflows$name)
    checks$info$github_workflows$conclusion [i] <- "fail"
    ci_out <- output_pkgchk_ci (checks)
    expect_false (ci_out$check_pass)
    expect_equal (
        ci_out$summary,
        " Package fails continuous integration checks."
    )

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

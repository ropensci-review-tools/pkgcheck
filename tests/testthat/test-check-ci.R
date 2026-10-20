# CI workflows and badges are only available with network access:
test_all <- identical (Sys.getenv ("RRT_TEST_ALL"), "true") ||
    identical (Sys.getenv ("GITHUB_JOB"), "test-coverage")
skip_if (!test_all)

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
    expect_length (ci_out$print, 3L)
    expect_named (ci_out$print, c ("msg_pre", "obj", "msg_post"))
    expect_length (ci_out$print$msg_pre, 1L)
    expect_length (ci_out$print$obj, 1L)
    expect_length (ci_out$print$msg_post, 1L)

    md <- ci_checks_to_md (checks)
    expect_type (md, "character")
    expect_length (grep ("GitHub Workflow Results", md), 1L)
    expect_gt (length (grep ("^\\|", md)), 3L)

    skip_on_os ("mac")

    i <- grep ("CMD", checks$info$github$workflows$name)
    checks$info$github$workflows$conclusion [i] <- "fail"
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

    checks$info$github$workflows$conclusion <- "success"
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

test_that ("ci results only use completed runs", {

    local_mocked_bindings (
        is_test_env = function () FALSE,
        pkginfo_url_from_desc = function (...) "https://github.com/a/b"
    )
    runs <- list (total_count = 3L, workflow_runs = data.frame (
        id = 1:3,
        name = c ("R-CMD-check.yaml", "R-CMD-check.yaml", "test-coverage.yaml"),
        status = c ("completed", "in_progress", "completed"),
        conclusion = c ("success", NA, "success"),
        head_sha = c ("aaaaaa1", "bbbbbb2", "aaaaaa1"),
        run_number = 1:3,
        created_at = c (
            "2026-10-18T12:00:00Z", "2026-10-18T12:30:00Z", "2026-10-18T12:00:00Z"
        )
    ))
    local_mocked_bindings (
        req_perform = function (...) NULL,
        resp_body_json = function (...) runs,
        .package = "httr2"
    )
    dat <- ci_results_gh (".")
    expect_identical (dat$conclusion, c ("success", "success"))
    expect_identical (dat$sha, c ("aaaaaa", "aaaaaa"))

    runs$workflow_runs$status <- "in_progress"
    expect_null (ci_results_gh ("."))
})

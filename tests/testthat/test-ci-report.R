skip_on_cran ()

test_that ("ci results in screen and markdown output", {

    checks <- make_check_data ()
    checks$checks$has_url <- TRUE
    checks$info$badges <- paste0 (
        "[![R-CMD-check](https://github.com/a/b/badge.svg)]",
        "(https://github.com/a/b)"
    )
    checks$info$github$workflows <- data.frame (
        id = 1:2,
        name = c ("R-CMD-check", "test-coverage"),
        conclusion = "success",
        sha = "abcdef",
        run_number = 1:2,
        date = "2026-10-16"
    )

    ci_out <- output_pkgchk_ci (checks)
    expect_true (ci_out$check_pass)
    msg_post <- ci_out$print$msg_post
    expect_length (msg_post, 1L)
    msg_lines <- strsplit (msg_post, "\n") [[1]]
    expect_identical (msg_lines [1], "**GitHub Workflow Results**")
    expect_length (grep ("^\\|", msg_lines), 4L) # header, rule, two rows

    pkg_env <- asNamespace ("pkgcheck")
    x <- capture.output (
        print_check_screen (checks, "ci", pkg_env),
        type = "message"
    )
    expect_length (grep ("Continuous Integration Badges", x), 1L)
    expect_true (all (msg_lines [-2] %in% x))

    withr::local_envvar (
        list ("PKGCHECK_TESTS" = "false", "GITHUB_ACTIONS" = "false")
    )
    expect_false (any (grepl ("Continuous Integration", get_gp_text (checks))))
    md <- get_extra_checks_text (checks, 2L)
    expect_length (grep ("Continuous Integration Badges", md), 1L)
    expect_length (grep ("GitHub Workflow Results", md), 1L)

    checks$info$github$workflows$conclusion [1] <- "failure"
    ci_out <- output_pkgchk_ci (checks)
    expect_false (ci_out$check_pass)
    expect_true (grepl ("|failure ", ci_out$print$msg_post, fixed = TRUE))
    md <- print_check_md (checks, "ci", pkg_env)
    expect_true (any (grepl (symbol_crs (), md, fixed = TRUE)))
})

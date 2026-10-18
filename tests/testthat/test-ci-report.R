skip_on_cran ()

test_that ("ci results in markdown report", {

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
    expect_identical (
        ci_out$print$msg_post [1:2],
        c ("**GitHub Workflow Results**", "")
    )
    expect_length (grep ("GitHub Workflow Results", ci_out$print$msg_post), 1L)

    withr::local_envvar (
        list ("PKGCHECK_TESTS" = "false", "GITHUB_ACTIONS" = "false")
    )
    md <- get_gp_text (checks)
    expect_type (md, "character")
    expect_length (grep ("Continuous Integration Badges", md), 1L)
    expect_true (checks$info$badges %in% md)

    expect_false ("ci" %in% output_has_print (checks))
    md <- get_extra_checks_text (checks, 2L)
    expect_false (any (grepl ("Continuous Integration Badges", md)))

    pkg_env <- asNamespace ("pkgcheck")
    md <- print_check_md (checks, "ci", pkg_env)
    expect_length (grep ("Continuous Integration Badges", md), 1L)
    checks_no_ci <- checks
    checks_no_ci$checks$ci <- NULL
    expect_null (print_check_screen (checks_no_ci, "ci", pkg_env))

    checks$info$github$workflows$conclusion [1] <- "failure"
    expect_identical (ci_checks_to_md (checks), "")
})

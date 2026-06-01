test_that ("check srr", {

    checks <- make_check_data_srr ()

    ci_out <- output_pkgchk_srr_okay (checks)

    expect_std_false <- function (chk) {
        expect_false (chk$check_pass)
        expect_length (chk$summary, 1L)
        expect_false (nzchar (chk$summary))
        expect_length (chk$print, 1L)
        expect_false (nzchar (chk$print))
    }
    expect_std_false (ci_out)

    ci_out <- output_pkgchk_srr_todo (checks)
    expect_equal (
        ci_out$summary,
        "This package still has TODO standards and can not be submitted"
    )
    ci_out$summary <- ""
    expect_std_false (ci_out)

    ci_out <- output_pkgchk_srr_missing (checks)
    expect_equal (
        ci_out$summary,
        "Some statistical standards are missing"
    )
    ci_out$summary <- ""
    expect_std_false (ci_out)

    checks$info$srr$okay <- TRUE
    ci_out <- output_pkgchk_srr_okay (checks)
    expect_true (ci_out$check_pass)
    expect_equal (
        ci_out$summary,
        "This is a statistical package which complies with all applicable standards"
    )

    most_in_one_pattern <- "should be documented in"
    ci_out <- output_pkgchk_srr_most_in_one_file (checks)
    expect_false (ci_out$check_pass)
    expect_true (grepl (most_in_one_pattern, ci_out$summary, fixed = TRUE))
    expect_length (ci_out$print, 1L)
    expect_false (nzchar (ci_out$print))

    orig_msg <- checks$info$srr$message
    checks$info$srr$message <- orig_msg [!grepl (most_in_one_pattern, orig_msg, fixed = TRUE)]
    ci_out <- output_pkgchk_srr_most_in_one_file (checks)
    expect_true (ci_out$check_pass)
    expect_false (nzchar (ci_out$summary))
    checks$info$srr$message <- orig_msg

    ci_out <- output_pkgchk_srr_general_only (checks)
    expect_true (ci_out$check_pass)
    expect_false (nzchar (ci_out$summary))
    expect_length (ci_out$print, 1L)
    expect_false (nzchar (ci_out$print))

    gen_only_msg <- "This package documents compliance only with general standards"
    checks$info$srr$message <- c (orig_msg, gen_only_msg)
    ci_out <- output_pkgchk_srr_general_only (checks)
    expect_false (ci_out$check_pass)
    expect_equal (
        ci_out$summary,
        "Package documents compliance only with general 'srr' standards"
    )
})

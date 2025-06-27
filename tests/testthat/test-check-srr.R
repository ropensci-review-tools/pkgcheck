# paths sometimes fail on windows/mac
skip_on_os ("windows")
skip_on_os ("mac")

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
})

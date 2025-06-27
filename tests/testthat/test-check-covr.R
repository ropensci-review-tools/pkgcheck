# paths sometimes fail on windows/mac
skip_on_os ("windows")
skip_on_os ("mac")

test_that ("check covr", {

    checks <- make_check_data_srr (goodpractice = TRUE)

    ci_out <- output_pkgchk_covr (checks)
    expect_type (ci_out, "list")
    expect_length (ci_out, 3L)
    expect_named (ci_out, c ("check_pass", "summary", "print"))

    expect_false (ci_out$check_pass)
    expect_equal (ci_out$summary, "Package coverage failed")
    expect_length (ci_out$print, 1L)
    expect_false (nzchar (ci_out$print))

    checks$goodpractice$covr <- list (pct_by_line = 12.3456)
    ci_out <- output_pkgchk_covr (checks)
    expect_false (ci_out$check_pass)
    expect_equal (
        ci_out$summary,
        "Package coverage is 12.3% (should be at least 75%)."
    )

    checks$goodpractice$covr <- list (pct_by_line = 92.3456)
    ci_out <- output_pkgchk_covr (checks)
    expect_true (ci_out$check_pass)
    expect_equal (ci_out$summary, "Package coverage is 92.3%.")
})

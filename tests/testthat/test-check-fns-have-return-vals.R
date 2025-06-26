test_that ("check fns have return values", {

    checks <- make_check_data ()

    ci_out <- output_pkgchk_fns_have_return_vals (checks)
    expect_type (ci_out, "list")
    expect_length (ci_out, 3L)
    expect_named (ci_out, c ("check_pass", "summary", "print"))

    expect_false (ci_out$check_pass)
    expect_true (grepl (
        "^The following functions have no documented return values",
        ci_out$summary,
    ))
    expect_length (ci_out$print, 1L)
    expect_false (nzchar (ci_out$print))

    # Only 1 fn, so text is singular:
    checks$checks$fns_have_return_vals <-
        checks$checks$fns_have_return_vals [1L]
    ci_out <- output_pkgchk_fns_have_return_vals (checks)
    expect_false (ci_out$check_pass)
    expect_true (grepl (
        "^The following function has no documented return value",
        ci_out$summary,
    ))
})

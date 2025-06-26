test_that ("check functions have exs", {

    checks <- make_check_data ()

    ci_out <- output_pkgchk_fns_have_exs (checks)
    expect_type (ci_out, "list")
    expect_length (ci_out, 3L)
    expect_named (ci_out, c ("check_pass", "summary", "print"))

    expect_false (ci_out$check_pass)
    expect_equal (
        ci_out$summary,
        "These functions do not have examples: [pkgstats_from_archive]."
    )
    expect_length (ci_out$print, 1L)
    expect_false (nzchar (ci_out$print))

    checks$checks$fns_have_exs ["pkgstats_from_archive"] <- TRUE
    ci_out <- output_pkgchk_fns_have_exs (checks)
    expect_true (ci_out$check_pass)
    expect_equal (ci_out$summary, "All functions have examples.")
})

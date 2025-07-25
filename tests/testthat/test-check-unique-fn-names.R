test_that ("check unique functions names", {

    checks <- make_check_data ()

    ci_out <- output_pkgchk_unique_fn_names (checks)
    expect_type (ci_out, "list")
    expect_length (ci_out, 4L)
    expect_named (ci_out, c ("check_pass", "summary", "print", "check_type"))

    expect_true (ci_out$check_pass)
    expect_length (ci_out$summary, 1L)
    expect_false (nzchar (ci_out$summary))
    expect_length (ci_out$print, 1L)
    expect_false (nzchar (ci_out$print))
    expect_equal (ci_out$check_type, "none_watch")

    dat <- data.frame (
        package = c ("one", "two"),
        version = 1:2,
        fn_name = c ("yes", "no")
    )
    checks$checks$unique_fn_names <- dat
    ci_out <- output_pkgchk_unique_fn_names (checks)
    expect_false (ci_out$check_pass)
    expect_equal (
        ci_out$summary,
        "Function names are duplicated in other packages"
    )
    expect_identical (sort (names (ci_out$print$obj)), sort (dat$fn_name))
})

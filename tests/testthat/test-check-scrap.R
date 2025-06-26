test_that ("check num imports", {

    checks <- make_check_data ()

    ci_out <- output_pkgchk_has_scrap (checks)

    expect_true (ci_out$check_pass)
    expect_length (ci_out$summary, 1L)
    expect_false (nzchar (ci_out$summary))
    expect_length (ci_out$print, 1L)
    expect_false (nzchar (ci_out$print))

    checks$checks$has_scrap <- "scrap"
    ci_out <- output_pkgchk_has_scrap (checks)
    expect_false (ci_out$check_pass)
    expect_equal (
        ci_out$summary,
        "Package contains unexpected files."
    )
    expect_length (ci_out$print, 3L)
    expect_named (ci_out$print, c ("msg_pre", "obj", "msg_post"))
    expect_true ("scrap" %in% ci_out$print$obj)
})

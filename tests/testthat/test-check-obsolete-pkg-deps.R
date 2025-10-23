test_that ("check num imports", {

    checks <- make_check_data ()

    ci_out <- output_pkgchk_obsolete_pkg_deps (checks)
    expect_type (ci_out, "list")
    expect_length (ci_out, 4L)
    expect_named (ci_out, c ("check_pass", "summary", "print", "check_type"))

    expect_true (ci_out$check_pass)
    expect_length (ci_out$summary, 1L)
    expect_false (nzchar (ci_out$summary))
    expect_length (ci_out$print, 1L)
    expect_false (nzchar (ci_out$print))
    expect_equal (ci_out$check_type, "none_watch")

    checks$checks$obsolete_pkg_deps <- "RCurl"
    ci_out <- output_pkgchk_obsolete_pkg_deps (checks)
    expect_false (ci_out$check_pass)
    expect_equal (
        ci_out$summary,
        "Package depends on the following obsolete packages: [RCurl]"
    )
    expect_length (ci_out$print, 3L)
    expect_named (ci_out$print, c ("msg_pre", "obj", "msg_post"))
})

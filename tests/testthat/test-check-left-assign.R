test_that ("check left and global assign", {

    checks <- make_check_data ()

    ci_out <- output_pkgchk_global_assign (checks)
    expect_type (ci_out, "list")
    expect_length (ci_out, 3L)
    expect_named (ci_out, c ("check_pass", "summary", "print"))

    expect_true (ci_out$check_pass)
    expect_length (ci_out$summary, 1L)
    expect_true (!nzchar (ci_out$summary))
    expect_length (ci_out$print, 1L)
    expect_true (!nzchar (ci_out$print))

    ci_out <- output_pkgchk_left_assign (checks)
    expect_type (ci_out, "list")
    expect_length (ci_out, 3L)
    expect_named (ci_out, c ("check_pass", "summary", "print"))

    expect_true (ci_out$check_pass)
    expect_length (ci_out$summary, 1L)
    expect_true (!nzchar (ci_out$summary))
    expect_length (ci_out$print, 1L)
    expect_true (!nzchar (ci_out$print))
})

test_that ("check pkgchk_left_assign fn", {

    # Need to modify code, so create new temp package:
    path <- srr::srr_stats_pkg_skeleton (pkg_name = "junk")
    checks <- list (pkg = list (path = path))
    res <- pkgchk_left_assign (checks)

    expect_type (res, "list")
    expect_length (res, 2L)
    expect_named (res, c ("global", "usage"))
    expect_type (res$global, "logical")
    expect_false (res$global)
    expect_type (res$usage, "integer")
    expect_named (res$usage, c ("<-", "="))
    expect_equal (as.integer (res$usage), c (2L, 0L))

    f <- fs::dir_ls (fs::path (path, "R"), regexp = "test\\.R$")
    txt <- c (
        readLines (f),
        "",
        "test_fn2 <- function() {",
        "  a = 1",
        "  b <<- 2",
        "  return (c (a, b))",
        "}"
    )
    writeLines (txt, f)

    res <- pkgchk_left_assign (checks)
    expect_true (res$global)
    expect_equal (as.integer (res$usage), c (3L, 1L))

    fs::dir_delete (path)
})

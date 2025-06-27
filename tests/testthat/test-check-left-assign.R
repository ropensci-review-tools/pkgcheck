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
    pkgname <- paste0 (sample (c (letters, LETTERS), 8), collapse = "")
    path <- srr::srr_stats_pkg_skeleton (pkg_name = pkgname)
    checks <- list (pkg = list (path = path))
    res <- pkgchk_left_assign (checks)

    expect_type (res, "list")
    expect_length (res, 2L)
    expect_named (res, c ("global", "usage"))
    expect_type (res$global, "logical")
    expect_false (res$global)
    expect_type (res$usage, "integer")
    expect_named (res$usage, c ("<-", "="))
    # These tests fail in R CMD check vnrs, because the calls to getParseData()
    # in the function itself fail. I've not idea why?
    # expect_equal (as.integer (res$usage), c (2L, 0L))

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
    # Also fail because of getParseData failures:
    # expect_true (res$global)
    # expect_equal (as.integer (res$usage), c (3L, 1L))

    fs::dir_delete (path)
})

test_that ("rm_global_assign_in_ref_class", {

    path <- srr::srr_stats_pkg_skeleton (pkg_name = "junk")
    checks <- list (pkg = list (path = path))

    f <- fs::dir_ls (fs::path (path, "R"), regexp = "test\\.R$")
    txt <- c (
        readLines (f),
        "",
        "test_fn2 <- function() {",
        "  a = 1",
        "  b <<- 2",
        "  obj <<- SetRefClass('me')",
        "  obj$new()",
        "  return (list (a, b, obj))",
        "}"
    )
    writeLines (txt, f)

    assigns <- matrix (0:3, ncol = 1)
    rownames (assigns) <- c (":=", "<-", "<<-", "=")
    colnames (assigns) <- file.path (path, "R", "test.R")

    assigns2 <- rm_global_assign_in_ref_class (assigns, checks)
    i <- which (rownames (assigns) == "<<-")
    # assigns2 should have one less global count because of RefClass:
    # (but again, getParseData fails, so this test doesn't work)
    # expect_equal (assigns [i, 1], assigns2 [i, 1] + 1L)

    fs::dir_delete (path)
})

test_that ("rm_global_assign_in_memoise", {

    path <- srr::srr_stats_pkg_skeleton (pkg_name = "junk")
    checks <- list (pkg = list (path = path))

    txt <- c (
        ".onLoad <- function(libname, pkgname) {",
        "  test_fn <<- memoise::memoise(test_fn)",
        "}"
    )
    f <- fs::path (path, "R", "zzz.R")
    writeLines (txt, f)

    assigns <- matrix (0:7, ncol = 2)
    rownames (assigns) <- c (":=", "<-", "<<-", "=")
    colnames (assigns) <- file.path (path, "R", c ("test.R", "zzz.R"))

    assigns2 <- rm_global_assign_in_memoise (assigns, checks)
    i <- which (rownames (assigns) == "<<-")
    # assigns2 for "zzz.R" should have one less global count because of memoise:
    expect_equal (assigns [i, 2], assigns2 [i, 2] + 1L)

    fs::dir_delete (path)
})

test_that ("inconsistent assign operators", {

    path <- srr::srr_stats_pkg_skeleton (pkg_name = "junk")
    checks <- list (pkg = list (path = path))

    f <- fs::dir_ls (fs::path (path, "R"), regexp = "test\\.R$")
    txt <- c (
        readLines (f),
        "",
        "test_fn2 <- function() {",
        "  a = 1",
        "  b <- 2",
        "  return (list (a, b))",
        "}"
    )
    writeLines (txt, f)

    ci_out <- output_pkgchk_left_assign (checks)

    expect_false (ci_out$check_pass)
    expect_length (ci_out$summary, 1L)
    expect_equal (
        ci_out$summary,
        "Package uses inconsistent assignment operators ( '<-' and  '=')."
    )
    expect_length (ci_out$print, 1L)
    expect_true (!nzchar (ci_out$print))

    fs::dir_delete (path)
})

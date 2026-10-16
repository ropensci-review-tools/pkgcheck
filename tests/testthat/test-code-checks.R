skip_on_cran ()

test_that ("left assign checks in non-interactive sessions", {

    withr::local_options (keep.source = FALSE)
    path <- withr::local_tempdir ()
    fs::dir_create (fs::path (path, "R"))
    writeLines (
        c ("f <- function (x) {", "    y = x", "    y", "}"),
        fs::path (path, "R", "f.R")
    )
    checks <- list (pkg = list (path = path))
    out <- pkgchk_left_assign (checks)
    expect_equal (unname (out$usage), c (1L, 1L))
})

test_that ("left assign checks with no R files", {

    path <- withr::local_tempdir ()
    fs::dir_create (fs::path (path, "R"))
    out <- pkgchk_left_assign (list (pkg = list (path = path)))
    expect_false (out$global)
    expect_equal (unname (out$usage), c (0L, 0L))
})

test_that ("left assign checks with no R directory", {

    out <- pkgchk_left_assign (list (pkg = list (path = withr::local_tempdir ())))
    expect_false (out$global)
})

test_that ("global assign checks without ctags", {

    local_mocked_bindings (
        get_ctags = function (...) stop ("no ctags"),
        .package = "pkgstats"
    )
    path <- withr::local_tempdir ()
    fs::dir_create (fs::path (path, "R"))
    writeLines (
        c ("f <- function () {", "    x <<- 1", "}"),
        fs::path (path, "R", "f.R")
    )
    out <- pkgchk_left_assign (list (pkg = list (path = path)))
    expect_true (out$global)
})

test_that ("percentiles with ties", {

    expect_equal (mid_rank_percentile (c (0, 0, 0, 1), 0), 0.375)
    expect_equal (mid_rank_percentile (c (1, 2, 3, 4), 5), 1)
    expect_equal (mid_rank_percentile (c (1, 2, 3, 4), 0), 0)
    expect_true (is.na (mid_rank_percentile (1:4, NA)))
})

test_that ("examples in Rd files with multiple keywords", {

    path <- withr::local_tempdir ()
    rd <- function (nm, extra) {
        f <- fs::path (path, paste0 (nm, ".Rd"))
        writeLines (c (
            paste0 ("\\name{", nm, "}"), paste0 ("\\alias{", nm, "}"),
            paste0 ("\\title{", nm, "}"), "\\description{x}", extra
        ), f)
        return (f)
    }
    f <- c (
        rd ("f", c ("\\keyword{misc}", "\\keyword{utilities}")),
        rd ("g", c ("\\keyword{misc}", "\\examples{g ()}")),
        rd ("h", c ("\\keyword{utilities}", "\\keyword{internal}"))
    )
    expect_identical (fns_have_exs (f), c (f = FALSE, g = TRUE))
})

test_that ("unreadable Rd files", {

    path <- withr::local_tempdir ()
    f <- fs::path (path, "man.Rd")
    fs::dir_create (f)
    msgs <- character (0L)
    rds <- withCallingHandlers (
        parse_rd_files (f),
        warning = function (w) {
            msgs <<- c (msgs, conditionMessage (w))
            invokeRestart ("muffleWarning")
        }
    )
    expect_length (rds, 0L)
    expect_true (any (grepl ("Error parsing Rd file", msgs)))
})

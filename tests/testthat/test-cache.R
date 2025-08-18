test_that ("check cache messages", {

    pkgname <- "cachecheckpkg"
    path <- srr::srr_stats_pkg_skeleton (pkg_name = pkgname)
    o <- capture.output (roxygen2::roxygenise (path), type = "message")

    checks <- pkgcheck (path, goodpractice = FALSE, use_cache = TRUE)

    # Re-run checks to load cached versions, which issues messages (see #289):
    msgs <- capture.output (
        checks <- pkgcheck (path, goodpractice = FALSE),
        type = "message"
    )
    expect_true (length (msgs) > 0L)
    expect_true (any (grepl ("Loading cached.*results", msgs)))
    expect_true (grepl (
        "To re-generate, call 'pkgcheck' function with 'use_cache = FALSE'",
        msgs [length (msgs)],
        fixed = TRUE
    ))

    msgs <- capture.output (
        checks <- pkgcheck (path, goodpractice = FALSE, use_cache = FALSE),
        type = "message"
    )
    expect_false (any (grepl ("Loading cached.*results", msgs)))

    fs::dir_delete (path)
})

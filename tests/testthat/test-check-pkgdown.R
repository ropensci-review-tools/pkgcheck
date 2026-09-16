skip_on_cran ()

test_that ("check functions have exs", {

    checks <- make_check_data (cleanup = FALSE)

    expect_named (
        checks$info$pkgdown,
        c ("num_rd_files", "yaml_path", "has_reference")
    )
    expect_type (checks$info$pkgdown$num_rd_files, "integer")
    expect_gt (checks$info$pkgdown$num_rd_files, 1L)
    expect_identical (checks$info$pkgdown$yaml_path, NA_character_)
    expect_type (checks$info$pkgdown$has_reference, "logical")

    expect_named (checks$checks$pkgdown, c ("check_pass", "summary"))
    expect_type (checks$checks$pkgdown$check_pass, "logical")
    expect_true (
        grepl ("Package has no", checks$checks$pkgdown$summary, fixed = TRUE)
    )

    out <- output_pkgchk_pkgdown (checks)
    expect_named (out, c ("check_pass", "summary", "print"))
    expect_false (out$check_pass)
    expect_true (grepl ("Package has no", out$summary, fixed = TRUE))
    expect_false (nzchar (out$print))

    # Then add a '_pkgdown.yaml' file:
    yaml_path <- fs::path (checks$pkg$path, "_pkgdown.yml")
    y <- c (
        "url: https://docs.ropensci.org/pkgstats"
    )
    writeLines (y, yaml_path)

    checks$info$pkgdown <- pkginfo_pkgdown (checks$pkg$path)
    checks$checks$pkgdown <- pkgchk_pkgdown (checks)

    expect_true (fs::file_exists (checks$info$pkgdown$yaml_path))
    expect_false (checks$info$pkgdown$has_reference)
    expect_false (checks$checks$pkgdown$check_pass)
    expect_true (grepl (
        "has no 'reference' grouping",
        checks$checks$pkgdown$summary,
        fixed = TRUE
    ))

    out <- output_pkgchk_pkgdown (checks)
    expect_identical (out$summary, checks$checks$pkgdown$summary)

    # Then add 'reference' grouping to '_pkgdown.yaml':
    y <- c (
        y,
        "",
        "reference:",
        "- title: Main functions"
    )
    writeLines (y, yaml_path)

    checks$info$pkgdown <- pkginfo_pkgdown (checks$pkg$path)
    checks$checks$pkgdown <- pkgchk_pkgdown (checks)

    expect_true (checks$info$pkgdown$has_reference)
    expect_true (checks$checks$pkgdown$check_pass)
    expect_false (nzchar (checks$checks$pkgdown$summary))

    fs::dir_delete (checks$pkg$path)
})


test_that ("pkgcheck", {

    skip_on_os ("windows")

    withr::local_options (list ("pkgcheck.cache_dir" =
                                file.path (tempdir (), "pkgcheck")))

    pkgname <- paste0 (
        sample (c (letters, LETTERS), 8),
        collapse = ""
    )
    d <- srr::srr_stats_pkg_skeleton (pkg_name = pkgname)

    x <- capture.output (
        roxygen2::roxygenise (d),
        type = "message"
    )

    expect_output (
        chk0 <- pkgcheck (d, goodpractice = FALSE)
    )

    nchks0 <- length (chk0$checks)
    md <- checks_to_markdown (chk0, render = FALSE)
    nchks0_md <- length (grep ("^\\-\\s\\:heavy", md))

    e <- new.env (parent = emptyenv ())
    e$pkgchk_new_check <- function (checks) {
        return (FALSE)
    }

    e$output_pkgchk_new_check <- function (checks) {
        out <- list (
            check_pass = checks$checks$new_check,
            summary = "",
            print = ""
        )

        out$summary <- ifelse (
            out$check_pass,
            "**NEW CHECK PASSES**",
            "**NEW CHECK DOES NOT PASS**"
        )
        out$print <- list (
            message = "New check output",
            obj = c ("new", "check")
        )

        return (out)
    }

    # Test that those checks are picked up in the checks$checks result:
    chk1 <- pkgcheck (d, goodpractice = FALSE, extra_env = e)
    nchks1 <- length (chk1$checks)
    expect_equal (nchks1, nchks0 + 1)

    # And that they are also rendered in the summary check output:
    md <- checks_to_markdown (chk1, render = FALSE)
    nchks1_md <- length (grep ("^\\-\\s\\:heavy", md))
    expect_equal (nchks1_md, nchks0_md + 1)
})

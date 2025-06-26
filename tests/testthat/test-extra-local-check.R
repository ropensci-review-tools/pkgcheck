test_that ("pkgcheck", {

    skip_on_os ("windows")

    chk0 <- make_check_data_srr ()

    nchks0 <- length (chk0$checks)
    md0 <- checks_to_markdown (chk0, render = FALSE)
    nchks0_md <- length (grep ("^\\-\\s\\:heavy", md0))

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
    # These envvars need to be set to ensure results are identical
    withr::local_envvar (list (
        "PKGCHECK_CACHE_DIR" =
            file.path (tempdir (), "pkgcheck")
    ))
    withr::local_envvar (list ("GITHUB_ACTIONS" = "true"))

    chk1 <- pkgcheck (chk0$pkg$path, goodpractice = FALSE, extra_env = e)

    nchks1 <- length (chk1$checks)
    expect_equal (nchks1, nchks0 + 1)

    # And that they are also rendered in the summary check output:
    md1 <- checks_to_markdown (chk1, render = FALSE)
    nchks1_md <- length (grep ("^\\-\\s\\:heavy", md1))
    expect_equal (nchks1_md, nchks0_md + 1)
})

# New checks have to be added to the `order_checks` function in `summarise-checks.R`.
# This test ensures that they are added by failing if not.

test_that ("order checks", {

    pkg_env <- asNamespace ("pkgcheck")
    pkg_fns <- ls (pkg_env)

    fns <- gsub (
        "^output\\_pkgchk\\_", "",
        grep ("^output\\_pkgchk\\_", pkg_fns, value = TRUE)
    )

    checks <- order_checks (fns)

    expect_length (fns, length (checks))
})

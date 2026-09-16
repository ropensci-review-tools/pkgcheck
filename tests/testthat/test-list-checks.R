test_that ("list-checks", {
    expect_message (
        chks <- list_pkgchecks (),
        "The following checks are currently implemented"
    )
    expect_length (chks, 28L)

    expect_silent (
        chks2 <- list_pkgchecks (quiet = TRUE)
    )
    expect_identical (chks, chks2)
})

# Every new function **MUST** be added to 'order_checks_sequence' defined in
# 'R/order-check-summaries.R'. This test confirms that.
test_that ("All checks listed in 'order_checks_sequence'", {

    # List all output fns:
    ptn <- "^output\\_pkgchk\\_"
    fns <- grep (
        ptn,
        ls (envir = asNamespace ("pkgcheck"), all.names = TRUE),
        value = TRUE
    )
    fns <- sort (gsub (ptn, "", fns))

    ords <- sort (order_checks_sequence)

    expect_identical (fns, ords)
})

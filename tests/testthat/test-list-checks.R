
test_that ("list-checks", {
    expect_message (
        chks <- list_pkgchecks (),
        "The following checks are currently implemented"
    )
    expect_length (chks, 17L)

    expect_silent (
        chks2 <- list_pkgchecks (quiet = TRUE)
    )
    expect_identical (chks, chks2)
})

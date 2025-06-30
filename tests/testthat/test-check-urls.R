# All of 'check-urls.R' is tested except error conditions in the
# `url_exists()` function.
test_that ("check urls", {

    # This URL must produce a small mocked file. All GitHub sites are much too
    # large!
    x <- "https://archlinux.org"
    chk <- httptest2::with_mock_dir ("url_true", {
        url_exists (x)
    })
    expect_true (chk)

    x <- "https://api.github.com/thisdoesnotexist"
    chk <- httptest2::with_mock_dir ("url_false", {
        url_exists (x)
    })
    expect_false (chk)
    regexp <- paste0 (
        "Requests for \\[",
        gsub ("\\/", "\\\\/", x),
        "\\] responded with HTTP status 404"
    )
    expect_warning (
        chk <- httptest2::with_mock_dir ("url_false", {
            url_exists (x, quiet = FALSE)
        }),
        regexp
    )
    expect_false (chk)

    retval <- "a"
    expect_warning (
        chk <- httptest2::with_mock_dir ("url_false", {
            url_exists (x, non_2xx_return_value = retval, quiet = FALSE)
        }),
        regexp
    )
    expect_identical (retval, chk)
})


test_that ("license-list", {

    expect_silent (x <- license_list ())
    expect_type (x, "character")
    expect_length (x, 33L)
})

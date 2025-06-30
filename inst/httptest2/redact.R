function (resp) {

    resp <- httptest2::gsub_response (
        resp,
        "api.github.com/",
        "",
        fixed = TRUE
    )

    resp <- httptest2::gsub_response (
        resp,
        "ropensci-review-tools/pkgcheck/git/trees/",
        "",
        fixed = TRUE
    )

    return (resp)
}

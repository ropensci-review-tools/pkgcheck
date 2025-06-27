function (resp) {

    resp <- httptest2::gsub_response (
        resp,
        "api.github.com/",
        "",
        fixed = TRUE
    )

    return (resp)
}

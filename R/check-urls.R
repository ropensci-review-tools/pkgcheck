#' Check that a package 'DESCRIPTION' file lists a valid URL.
#'
#' @noRd
pkgchk_has_url <- function (checks) {

    out <- length (checks$pkg$url) > 0L
    if (out) {
        out <- !is.na (checks$pkg$url) & nzchar (checks$pkg$url)
    }

    return (out)
}

#' Check that a package 'DESCRIPTION' file lists a valid URL in the "BugReports"
#' field.
#'
#' @noRd
pkgchk_has_bugs <- function (checks) {

    out <- length (checks$pkg$BugReports) > 0L
    if (out) {
        out <- !is.na (checks$pkg$BugReports) & nzchar (checks$pkg$BugReports)
    }

    return (out)
}

output_pkgchk_has_url <- function (checks) {

    out <- list (
        check_pass = checks$checks$has_url,
        summary = "",
        print = ""
    ) # no print method

    out$summary <- paste0 (
        "'DESCRIPTION' ",
        ifelse (out$check_pass, "has", "does not have"),
        " a URL field."
    )

    return (out)
}

output_pkgchk_has_bugs <- function (checks) {

    out <- list (
        check_pass = checks$checks$has_bugs,
        summary = "",
        print = ""
    ) # no print method

    out$summary <- paste0 (
        "'DESCRIPTION' ",
        ifelse (out$check_pass, "has", "does not have"),
        " a BugReports field."
    )

    return (out)
}

#' Bob Rudis's URL checker function, updated for httr2
#'
#' @param x a single URL
#' @param non_2xx_return_value what to do if the site exists but the HTTP status
#' code is not in the `2xx` range. Default is to return `FALSE`.
#' @param quiet if not `FALSE`, then every time the `non_2xx_return_value`
#' condition arises a warning message will be displayed. Default is `FALSE`.
#' @param ... other params (`timeout()` would be a good one) passed directly to
#' \pkg{httr2} functions.
#'
#' @note
#' https://stackoverflow.com/questions/52911812/check-if-url-exists-in-r
#' @noRd
url_exists <- function (x, non_2xx_return_value = FALSE, quiet = TRUE, ...) {

    req <- httr2::request (x)
    resp <- tryCatch (
        httr2::req_perform (req),
        error = function (e) {
            e
        },
        interrupt = function (e) {
            stop ("Terminated by user", call. = FALSE)
        }
    )

    if (!inherits (resp, "httr2_error")) {
        status <- httr2::resp_status (resp)
    } else {
        status <- resp$resp$status_code
    }
    if (is.null (status)) {
        return (FALSE)
    }

    if (status / 200 > 1) {
        if (!quiet) {
            warning (paste0 (
                "Requests for [",
                x,
                "] responded with HTTP status ",
                status
            ))
        }
        return (non_2xx_return_value)
    }

    return (TRUE)
}

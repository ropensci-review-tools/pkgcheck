
pkgchk_has_url <- function (checks) {
    out <- length (checks$pkg$url) > 0L
    if (out) {
        out <- !is.na (checks$pkg$url)
    }

    return (out)
}

pkgchk_has_bugs <- function (checks) {
    out <- length (checks$pkg$BugReports) > 0L
    if (out) {
        out <- !is.na (checks$pkg$BugReports)
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

#' Bob Rudis's URL checker function
#'
#' @param x a single URL
#' @param non_2xx_return_value what to do if the site exists but the HTTP status
#' code is not in the `2xx` range. Default is to return `FALSE`.
#' @param quiet if not `FALSE`, then every time the `non_2xx_return_value`
#' condition arises a warning message will be displayed. Default is `FALSE`.
#' @param ... other params (`timeout()` would be a good one) passed directly to
#' `httr::HEAD()` and/or `httr::GET()`
#'
#' @note
#' https://stackoverflow.com/questions/52911812/check-if-url-exists-in-r
#' @noRd
url_exists <- function (x, non_2xx_return_value = FALSE, quiet = FALSE, ...) {

    # you don't need thse two functions if you're already using `purrr`
    # but `purrr` is a heavyweight compiled pacakge that introduces
    # many other "tidyverse" dependencies and this doesnt.

    capture_error <- function (code, otherwise = NULL, quiet = TRUE) {
        tryCatch (
            list (result = code, error = NULL),
            error = function (e) {
                if (!quiet) {
                    message ("Error: ", e$message)
                }

                list (result = otherwise, error = e)
            },
            interrupt = function (e) {
                stop ("Terminated by user", call. = FALSE)
            }
        )
    }

    safely <- function (.f, otherwise = NULL, quiet = TRUE) {
        function (...) capture_error (.f (...), otherwise, quiet)
    }

    sHEAD <- safely (httr::HEAD) # nolint
    sGET <- safely (httr::GET) # nolint

    # Try HEAD first since it's lightweight
    res <- sHEAD (x, ...)

    if (is.null (res$result) ||
        ((httr::status_code (res$result) %/% 200) != 1)) {
        res <- sGET (x, ...)

        if (is.null (res$result)) {
            return (NA)
        } # or whatever you want to return on "hard" errors

        if (((httr::status_code (res$result) %/% 200) != 1)) {
            if (!quiet) {
                warning (paste0 (
                    "Requests for [",
                    x,
                    "] responded but without an HTTP status ",
                    "code in the 200-299 range"
                ))
            }
            return (non_2xx_return_value)
        }

        return (TRUE)
    } else {
        return (TRUE)
    }
}

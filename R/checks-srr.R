
pkgchk_srr <- function (checks) {

    checks$info$srr$okay
}

output_pkgchk_srr_todo <- function (checks) {

    out <- list (
        check_pass = !any (grepl (
            "still has TODO standards",
            checks$info$srr$message
        )),
        summary = grep ("still has TODO standards",
            checks$info$srr$message,
            value = TRUE
        ),
        print = ""
    )

    return (out)
}

output_pkgchk_srr_missing <- function (checks) {

    srr <- checks$info$srr

    check_pass <- !any (grepl (
        "following standards \\[v.*\\] are missing",
        srr$message
    ))

    out <- list (
        check_pass = check_pass,
        summary = "",
        print = ""
    )

    if (!out$check_pass) {
        out$summary <- "Statistical standards are missing"
    }

    return (out)
}

print_srr <- function (x) {

    cli::cli_h2 ("rOpenSci Statistical Standards")
    ncats <- length (x$info$srr$categories) # nolint
    cli::cli_alert_info ("The package is in the following {ncats} categor{?y/ies}:") # nolint
    cli::cli_li (x$info$srr$categories)
    cli::cli_text ("")
    cli::cli_alert_info ("Compliance with rOpenSci statistical standards:")

    if (x$info$srr$okay) {
        cli::cli_alert_success (x$info$srr$message)
    } else {
        cli::cli_alert_danger (x$info$srr$message [1])
        if (length (x$info$srr$message) > 1) {
            m <- x$info$srr$message [-1]
            if (grepl ("missing from your code", m [1])) {
                cli::cli_text (m [1])
                cli::cli_text ("")
                m <- paste0 (m [which (m != "")] [-1], collapse = ", ")
                cli::cli_text (paste0 (m, "."))
            }
        }
        return ()
    }

    if (!is.null (x$info$srr$missing_stds)) {
        cli::cli_alert_warning ("The following standards are missing:")
        cli::cli_li (x$info$srr$missing_stds)
    }

    cli::cli_alert_info ("'srr' report is at [{x$info$srr$report_file}].")
    message ("")
}

#' Format `srr` checks in markdown
#' @param checks Result of main \link{pkgcheck} function
#' @noRd
srr_checks_to_md <- function (checks) {

    if (is.null (checks$info$srr)) {
        return (NULL)
    }

    cat_plural <- ifelse (
        length (checks$info$srr$categories == 1),
        "category",
        "categories"
    )
    srr_msg <- ifelse (checks$info$srr$okay,
        paste0 (symbol_tck (), " ", checks$info$srr$message),
        paste0 (symbol_crs (), " ", checks$info$srr$message)
    )

    c (
        paste0 (
            "### 1. rOpenSci Statistical Standards ",
            "([`srr` package]",
            "(https://github.com/ropensci-review-tools/srr))"
        ),
        "",
        paste0 ("This package is in the following ", cat_plural, ":"),
        "",
        paste0 ("- *", checks$info$srr$categories, "*"),
        "",
        srr_msg,
        "",
        paste0 (
            "Click [here to see the report of author-reported ",
            "standards compliance of the package with links to ",
            "associated lines of code](",
            report_file (checks),
            "), which can be re-generated locally by running the ",
            "[`srr_report()` function]",
            "(https://docs.ropensci.org/srr/reference/srr_report.html) ",
            "from within a local clone of the repository."
        ),
        "",
        "---",
        ""
    )
}

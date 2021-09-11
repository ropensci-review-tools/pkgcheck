#' @export
print.pkgcheck <- function (x, ...) {

    requireNamespace ("goodpractice")

    cli::cli_h1 (paste0 (x$package, " ", x$version))
    message ("")

    print_summary (x)

    print_git (x)
    if (!is.null (x$srr))
        print_srr (x)
    print_structure (x)

    cli::cli_h3 ("All statistics")
    x$pkgstats$value <- round (x$pkgstats$value, digits = 1)
    x$pkgstats$percentile <- round (x$pkgstats$percentile, digits = 1)
    print (x$pkgstats)
    message ("")

    if ("network_file" %in% names (x)) {
        cli::cli_alert_info ("Package network diagram is at [{x$network_file}]")
    } else {
        cli::cli_alert_warning ("This package has no function call network")
    }
    message ("")

    cli::cli_h2 ("goodpractice")
    print (x$gp)

    # ---- Add new print methods here ---
    # see https://github.com/ropensci-review-tools/pkgcheck/pull/27
    # for an example of how to add new checks
    has_misc_checks <- length (x$scrap > 0L) # Modify when more checks are added
    if (has_misc_checks)
        cli::cli_h2 ("Other checks")
    print_scrap (x)
    # --- End add new checks

    cli::cli_h2 ("Package Versions")
    cli::cli_dl (x$pkg_versions)
}

#' @export
summary.pkgcheck <- function (object, ...) {

    cli::cli_h1 (paste0 (object$package, " ", object$version))
    message ("")

    print_summary (object)
}

print_summary <- function (x) {

    # collate_checks is in collate-checks.R
    s <- collate_checks (x)

    okay <- attr (s, "checks_okay")
    s <- grep ("^\\-", s, value = TRUE)

    for (i in s) {

        msg <- strsplit (i, "(mark|\\_x):\\s+") [[1]] [2]
        if (grepl ("heavy_check_mark", i)) {
            cli::cli_alert_success (msg)
        } else {
            cli::cli_alert_danger (msg)
        }
    }

    message ("")
    cli::cli_alert_info ("Current status:")
    if (okay) {
        cli::cli_alert_success ("This package may be submitted")
    } else {
        cli::cli_alert_danger ("This package is not ready to be submitted")
    }

    message ("")
}

print_git <- function (x) {

    cli::cli_h2 ("git")

    since <- strftime (x$git$since, "%d-%m-%Y") # nolint
    gitstats <- c ("HEAD: {substring (x$git$HEAD, 1, 8)}",
                   "Default branch: {x$git$branch}",
                   "Number of commits: {x$git$num_commits}",
                   "First commit: {since}",
                   "Number of authors: {x$git$num_authors}")
    cli::cli_li (gitstats)
    message ("")

}

print_structure <- function (x) {

    cli::cli_h2 ("Package Structure")

    cli::cli_alert_info ("Package uses the following languages:")
    cli::cli_li (x$summary$languages)
    message ("")
    cli::cli_alert_info ("Package has")

    s <- c ("{x$summary$num_authors} author{?s}",
            "{x$summary$num_vignettes} vignette{?s}")
    if (x$summary$num_data == 0L) {
        s <- c (s, "No internal data")
    } else {
        s <- c (s, "{x$summary$num_data} internal data file{?s}")
    }
    s <- c (s,
            "{x$summary$imported_pkgs} imported package{?s}")

    if (x$summary$num_exported_fns == 0L) {
        s <- c (s, paste0 ("No exported functions"))
    } else {
        s <- c (s,
            paste0 ("{x$summary$num_exported_fns} exported function{?s} ",
                    "(median {x$summary$loc_exported_fns} lines of code)"))
    }
    if (x$summary$num_non_exported_fns == 0L) {
        s <- c (s, paste0 ("No non-exported functions"))
    } else {
        s <- c (s,
                paste0 ("{x$summary$num_non_exported_fns} non-exported ",
                        "function{?s} (median ",
                        "{x$summary$loc_non_exported_fns} lines of code)"))
    }

    if (x$summary$num_src_fns > 0) {
        langs <- vapply (strsplit (x$summary$languages, ":"), function (i)
                         i [1], character (1))
        langs <- paste0 (langs [langs != "R"], collapse = ", ")
        s <- c (s,
                paste0 ("{x$summary$num_src_fns} {langs} functions ",
                        "(median {x$summary$loc_src_fns} lines of code)"))
    }

    s <- c (s,
            "{x$summary$num_params_per_fn} parameters per function (median)")


    cli::cli_li (s)
}

print_srr <- function (x) {

    cli::cli_h2 ("rOpenSci Statistical Standards")
    ncats <- length (x$srr$categories) # nolint
    cli::cli_alert_info ("The package is in the following {ncats} categor{?y/ies}:") # nolint
    cli::cli_li (x$srr$categories)
    cli::cli_text ("")
    cli::cli_alert_info ("Compliance with rOpenSci statistical standards:")

    if (x$srr$okay) {
        cli::cli_alert_success (x$srr$message)
    } else {
        cli::cli_alert_danger (x$srr$message [1])
        if (length (x$srr$message) > 1) {
            m <- x$srr$message [-1]
            if (grepl ("missing from your code", m [1])) {
                cli::cli_text (m [1])
                cli::cli_text ("")
                m <- paste0 (m [which (m != "")] [-1], collapse = ", ")
                cli::cli_text (m)
            }
        }
        return ()
    }

    if (!is.null (x$srr$missing_stds)) {
        cli::cli_alert_warning ("The following standards are missing:")
        cli::cli_li (x$srr$missing_stds)
    }

    cli::cli_alert_info ("'srr' report is at [{x$srr$report_file}]")
    message ("")
}

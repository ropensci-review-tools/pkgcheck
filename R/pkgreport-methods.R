#' @export
print.pkgreport <- function (x, ...) {

    cli::cli_h1 (paste0 (x$package, " ", x$version))
    message ("")

    print_summary (x)

    print_git (x)
    print_srr (x)
    print_structure (x)

    cli::cli_h3 ("All statistics")
    x$pkgstats$value <- round (x$pkgstats$value, digits = 1)
    x$pkgstats$percentile <- round (x$pkgstats$percentile, digits = 1)
    print (x$pkgstats)
    message ("")
    cli::cli_alert_info ("Package network diagram is at [{x$network_file}]")
    message ("")


    cli::cli_h2 ("goodpractice")
    print (x$gp)
}

#' @export
summary.pkgreport <- function (x, ...) {

    cli::cli_h1 (paste0 (x$package, " ", x$version))
    message ("")

    print_summary (x)
}

print_summary <- function (x) {

    cli::cli_h2 ("Package Summary")

    okay <- TRUE
    if (x$file_list$uses_roxy) {
        cli::cli_alert_success ("Package uses 'roxygen2'")
    } else {
        cli::cli_alert_danger ("Package does not use 'roxygen2'")
        okay <- FALSE
    }

    if (x$file_list$has_contrib) {
        cli::cli_alert_success ("Package has a 'contributing.md' file")
    } else {
        cli::cli_alert_danger ("Package does not have a 'contributing.md' file")
        okay <- FALSE
    }

    if (all (x$fns_have_exs)) {
        cli::cli_alert_success ("All exported functions have examples")
    } else {
        noex <- names (x$fns_have_exs [which (!x$fns_have_exs)])
        cli::cli_alert_danger ("These exported functions do not have examples [{noex}]")
        okay <- FALSE
    }

    if (x$file_list$has_url) {
        cli::cli_alert_success ("Package 'DESCRIPTION' has a URL field")
    } else {
        cli::cli_alert_danger ("Package 'DESCRIPTION' has no URL field")
        okay <- FALSE
    }

    if (x$file_list$has_bugs) {
        cli::cli_alert_success ("Package 'DESCRIPTION' has a BugReports field")
    } else {
        cli::cli_alert_danger ("Package 'DESCRIPTION' has no BugReports field")
        okay <- FALSE
    }

    if (x$left_assigns$global)
        cli::cli_alert_danger ("Package uses a global assignment operator ('<<-')")
    if (all (x$left_assign$usage > 0)) {
        la <- x$left_assign$usage
        cli::cli_alert_danger (paste0 ("Package uses inconsistent left-assign ",
                                       "operators (", la [1], " '<-' and ",
                                       la [2], " '=')"))
        okay <- FALSE
    }

    if (!is.null (x$badges)) {
        cli::cli_alert_success ("Package has continuous integration checks")
    } else {
        cli::cli_alert_danger ("Package does not have continuous integration checks")
        okay <- FALSE
    }

    coverage <- round (x$gp$covr$pct_by_line, digits = 1)
    if (x$gp$covr$pct_by_line >= 75) {
        cli::cli_alert_success ("Package coverage is {coverage}%")
    } else {
        cli::cli_alert_danger ("Package coverage is {coverage}% (should be at least 75%)")
        okay <- FALSE
    }

    nerr <- length (x$gp$rcmdcheck$errors)
    if (nerr == 0) {
        cli::cli_alert_success ("R CMD check found no errors")
    } else {
        cli::cli_alert_danger ("R CMD check found {nwarn} error{?s}")
        okay <- FALSE
    }

    nwarn <- length (x$gp$rcmdcheck$warnings)
    if (nwarn == 0) {
        cli::cli_alert_success ("R CMD check found no warnings")
    } else {
        cli::cli_alert_danger ("R CMD check found {nwarn} warning{?s}")
        okay <- FALSE
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

    since <- strftime (x$git$since, "%d-%m-%Y")
    gitstats <- c ("HEAD: {substring (x$git$HEAD, 1, 8)}",
                   "Default branch: {x$git$branch}",
                   "Num commits: {x$git$num_commits}",
                   "First commit: {since}",
                   "Num authors: {x$git$num_authors}")
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
            "{x$summary$imported_pkgs} imported package{?s}",
            paste0 ("{x$summary$num_exported_fns} exported function{?s} ",
                    "(median {x$summary$loc_exported_fns} lines of code)"),
            paste0 ("{x$summary$num_non_exported_fns} non-exported function{?s} ",
                    "(median {x$summary$loc_non_exported_fns} lines of code)"))
    if (x$summary$num_src_fns > 0) {
        langs <- vapply (strsplit (x$summary$languages, ":"), function (i)
                         i [1], character (1))
        langs <- paste0 (langs [langs != "R"], collapse = ", ")
        s <- c (s,
                paste0 ("{x$summary$num_src_fns} {langs} functions ",
                        "(median {x$summary$loc_src_fns} lines of code)"))
    }
    s <- c (s,
            "{x$summary$num_params_per_fn} parameters per function")


    cli::cli_li (s)
}

print_srr <- function (x) {

    cli::cli_h2 ("Statistical Standards")

    if (x$srr$okay) {
        cli::cli_alert_success (x$srr$message)
    } else {
        cli::cli_alert_danger (x$srr$message)
        return ()
    }

    srr <- ifelse (length (x$srr$categories) == 1,
                   "Category",
                   "Categories")
    cli::cli_alert_info (srr)
    cli::cli_li (x$srr$categories)

    if (!is.null (x$srr$missing_stds)) {
        cli::cli_alert_warning ("The following standards are missing:")
        cli::cli_li (x$srr$missing_stds)
    }

    cli::cli_alert_info ("'srr' report is at [{x$srr$report_file}]")
    message ("")
}

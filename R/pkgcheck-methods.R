#' @export
print.pkgcheck <- function (x, ...) {

    requireNamespace ("goodpractice")

    cli::cli_h1 (paste0 (x$package$name, " ", x$package$version))
    message ("")

    print_summary (x)

    print_git (x)
    if (!is.null (x$info$srr))
        print_srr (x)
    print_structure (x)

    pkg_fns <- ls (as.environment ("package:pkgcheck"),
                   all.names = TRUE)
    output_fns <- gsub ("^output\\_pkgchk\\_", "",
                        grep ("^output\\_pkgchk\\_", pkg_fns, value = TRUE))
    out <- lapply (output_fns, function (i) print_check (x, i))
    out <- do.call (c, out [which (nchar (out) > 0L)])

    cli::cli_h3 ("All statistics")
    x$info$pkgstats$value <- round (x$info$pkgstats$value, digits = 1)
    x$info$pkgstats$percentile <- round (x$info$pkgstats$percentile, digits = 1)
    print (x$info$pkgstats)
    message ("")

    if ("network_file" %in% names (x$info)) {
        cli::cli_alert_info ("Package network diagram is at [{x$info$network_file}].")
    } else {
        cli::cli_alert_warning ("This package has no function call network.")
    }
    message ("")

    cli::cli_h2 ("goodpractice")
    print (x$goodpractice)

    # ---- Add new print methods here ---
    # see https://github.com/ropensci-review-tools/pkgcheck/pull/27
    # for an example of how to add new checks
    has_misc_checks <- length (x$checks$scrap > 0L) # Modify when more checks are added
    if (has_misc_checks) {
        cli::cli_h2 ("Other checks")
        print_check_screen (x, "has_scrap")
    }
    # --- End add new checks

    cli::cli_h2 ("Package Versions")
    cli::cli_dl (x$meta)
}

#' @export
summary.pkgcheck <- function (object, ...) {

    cli::cli_h1 (paste0 (object$package$name, " ", object$package$version))
    message ("")

    print_summary (object)
}

print_summary <- function (x) {

    # summarise_all_checks is in summarise-checks.R
    s <- summarise_all_checks (x)

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
        cli::cli_alert_success ("This package may be submitted.")
    } else {
        cli::cli_alert_danger ("This package is not ready to be submitted.")
    }

    message ("")
}

print_git <- function (x) {

    cli::cli_h2 ("git")

    since <- strftime (x$info$git$since, "%d-%m-%Y") # nolint
    gitstats <- c ("HEAD: {substring (x$info$git$HEAD, 1, 8)}",
                   "Default branch: {x$info$git$branch}",
                   "Number of commits: {x$info$git$num_commits}",
                   "First commit: {since}",
                   "Number of authors: {x$info$git$num_authors}")
    cli::cli_li (gitstats)
    message ("")

}

print_structure <- function (x) {

    pkg_summ <- x$package$summary

    cli::cli_h2 ("Package Structure")

    cli::cli_alert_info ("Package uses the following languages:")
    cli::cli_li (pkg_summ$languages)
    message ("")
    cli::cli_alert_info ("Package has")

    s <- c ("{pkg_summ$num_authors} author{?s}.",
            "{pkg_summ$num_vignettes} vignette{?s}.")
    if (pkg_summ$num_data == 0L) {
        s <- c (s, "No internal data")
    } else {
        s <- c (s, "{pkg_summ$num_data} internal data file{?s}.")
    }
    s <- c (s,
            "{pkg_summ$imported_pkgs} imported package{?s}.")

    if (pkg_summ$num_exported_fns == 0L) {
        s <- c (s, paste0 ("No exported functions."))
    } else {
        s <- c (s,
            paste0 ("{pkg_summ$num_exported_fns} exported function{?s} ",
                    "(median {pkg_summ$loc_exported_fns} lines of code)."))
    }
    if (pkg_summ$num_non_exported_fns == 0L) {
        s <- c (s, paste0 ("No non-exported functions."))
    } else {
        s <- c (s,
                paste0 ("{pkg_summ$num_non_exported_fns} non-exported ",
                        "function{?s} (median ",
                        "{pkg_summ$loc_non_exported_fns} lines of code)."))
    }

    if (pkg_summ$num_src_fns > 0) {
        langs <- vapply (strsplit (pkg_summ$languages, ":"), function (i)
                         i [1], character (1))
        langs <- paste0 (langs [langs != "R"], collapse = ", ")
        s <- c (s,
                paste0 ("{pkg_summ$num_src_fns} {langs} functions ",
                        "(median {pkg_summ$loc_src_fns} lines of code)."))
    }

    s <- c (s,
            "{pkg_summ$num_params_per_fn} parameters per function (median).")


    cli::cli_li (s)
}

#' Generic function to print checks based on result of corresponding
#' `output_pkgchk_` function.
#'
#' @param checks Full result of `pkgcheck()` call
#' @param what Name of check which must also correspond to an internal function
#' named `output_pkgchk_<name>`.
#' @return Check formatted to apepar in `print` method
#' @noRd
print_check <- function (checks, what) {

    pkg_env <- asNamespace ("pkgcheck")
    pkg_fns <- ls (envir = pkg_env)

    output_fn <- paste0 ("output_pkgchk_", what)
    if (!output_fn %in% pkg_fns)
        return (NULL)

    chk_output <- do.call (output_fn, list (checks), envir = pkg_env)

    return (chk_output$print)
}

print_check_screen <- function (checks, what) {

    pkg_env <- as.environment ("package:pkgcheck")

    output_fn <- get (paste0 ("output_pkgchk_", what),
                      envir = pkg_env)

    chk_output <- do.call (output_fn, list (checks))

    if (chk_output$check_pass) {
        cli::cli_alert_success (chk_output$print$message)
    } else {
        cli::cli_alert_danger (chk_output$print$message)
    }

    if (is.vector (chk_output$print$obj)) {

        cli::cli_ul ()
        cli::cli_li (chk_output$print$obj)
        cli::cli_end ()
    }
}

print_check_md <- function (checks, what) {

    pkg_env <- as.environment ("package:pkgcheck")

    output_fn <- get (paste0 ("output_pkgchk_", what),
                      envir = pkg_env)

    chk_output <- do.call (output_fn, list (checks))

    out <- NULL

    if (!chk_output$check_pass) {

        out <- c ("",
                  paste0 (symbol_crs (), " ", chk_output$print$message))

    } else if (nchar (chk_output$print$message) > 0L) {

        out <- c ("",
                  paste0 (symbol_tck (), " ", chk_output$print$message))
    }

    if (!is.null (out) & is.vector (chk_output$print$obj)) {

        out <- c (out,
                  "",
                  paste0 ("- ", chk_output$print$obj),
                  "")
    }

    return (out)
}

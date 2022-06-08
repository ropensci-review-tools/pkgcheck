
#' Generic print method for 'pkgcheck' objects.
#' @param x A 'pkgcheck' object to be printed.
#' @param deps If 'TRUE', include details of dependency packages and function
#' usage.
#' @param ... Further arguments pass to or from other methods (not used here).
#' @export
print.pkgcheck <- function (x, deps = FALSE, ...) {

    requireNamespace ("goodpractice")

    cli::cli_h1 (paste0 (x$pkg$name, " ", x$pkg$version))
    message ("")

    print_summary (x)

    print_git (x)
    if (!is.null (x$info$srr)) {
        print_srr (x)
    }
    print_structure (x)

    if (deps) {
        print_deps (x)
    }

    pkg_fns <- ls ("package:pkgcheck", envir = loadNamespace ("pkgcheck"))
    output_fns <- gsub (
        "^output\\_pkgchk\\_", "",
        grep ("^output\\_pkgchk\\_", pkg_fns, value = TRUE)
    )
    has_gp <- "goodpractice" %in% names (x)
    if (!has_gp) {
        output_fns <- output_fns [which (!grepl ("covr", output_fns))]
    }
    out <- lapply (output_fns, function (i) print_check (x, i))
    out <- do.call (c, out [which (nchar (out) > 0L)])

    cli::cli_h2 ("Package statistics")
    x$info$pkgstats$value <- round (x$info$pkgstats$value, digits = 1)
    x$info$pkgstats$percentile <- round (x$info$pkgstats$percentile, digits = 1)
    print (x$info$pkgstats)
    message ("")

    if ("network_file" %in% names (x$info)) {
        cli::cli_alert_info (
            "Package network diagram is at [{x$info$network_file}]."
        )
    } else {
        cli::cli_alert_warning (
            "This package has no function call network."
        )
    }
    message ("")

    cli::cli_h2 ("goodpractice")
    if (has_gp) {
        print (x$goodpractice)
    } else {
        cli::cli_alert_info ("'goodpractice' not included with these checks")
    }

    pkg_env <- env2namespace ("pkgcheck")
    if (sum (misc_check_counts (x)) > 0L) {
        cli::cli_h2 ("Other checks")
        print_check_screen (x, "unique_fn_names", pkg_env)
        print_check_screen (x, "has_scrap", pkg_env)
        print_check_screen (x, "renv_activated", pkg_env)
        print_check_screen (x, "obsolete_pkg_deps", pkg_env)
    }

    # additional external checks:
    extra <- extra_check_prints_from_env (x)
    if (length (extra$env) > 0L) {
        if (sum (misc_check_counts) == 0L) {
            cli::cli_h2 ("Other checks")
        }
        for (e in extra$env) {
            for (p in extra$prints) {
                print_check_screen (x, p, e)
            }
        }
    }

    cli::cli_h2 ("Package Versions")
    cli::cli_dl (x$meta)
}

# internal misc checks; modify condition when more checks are added
misc_check_counts <- function (x) {

    c (
        has_scrap = length (x$checks$has_scrap),
        obsolete_pkg_deps = length (x$checks$obsolete_pkg_deps),
        unique_fn_names = nrow (x$checks$unique_fn_names),
        renv_activated = as.integer (x$checks$renv_activated)
    )
}

#' @export
summary.pkgcheck <- function (object, ...) {

    cli::cli_h1 (paste0 (object$pkg$name, " ", object$pkg$version))
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
    gitstats <- c (
        "HEAD: {substring (x$info$git$HEAD, 1, 8)}",
        "Default branch: {x$info$git$branch}",
        "Number of commits: {x$info$git$num_commits}",
        "First commit: {since}",
        "Number of authors: {x$info$git$num_authors}"
    )
    cli::cli_li (gitstats)
    message ("")
}

print_structure <- function (x) {

    pkg_summ <- x$pkg$summary

    cli::cli_h2 ("Package Structure")

    cli::cli_alert_info ("Package uses the following languages:")
    cli::cli_li (pkg_summ$languages)
    message ("")
    cli::cli_alert_info ("Package has")

    s <- c (
        "{pkg_summ$num_authors} author{?s}.",
        "{pkg_summ$num_vignettes} vignette{?s}."
    )
    if (pkg_summ$num_data == 0L) {
        s <- c (s, "No internal data")
    } else {
        s <- c (s, "{pkg_summ$num_data} internal data file{?s}.")
    }
    s <- c (
        s,
        "{pkg_summ$imported_pkgs} imported package{?s}."
    )

    if (pkg_summ$num_exported_fns == 0L) {
        s <- c (s, paste0 ("No exported functions."))
    } else {
        s <- c (
            s,
            paste0 (
                "{pkg_summ$num_exported_fns} exported function{?s} ",
                "(median {pkg_summ$loc_exported_fns} lines of code)."
            )
        )
    }
    if (pkg_summ$num_non_exported_fns == 0L) {
        s <- c (s, paste0 ("No non-exported functions."))
    } else {
        s <- c (
            s,
            paste0 (
                "{pkg_summ$num_non_exported_fns} non-exported ",
                "function{?s} (median ",
                "{pkg_summ$loc_non_exported_fns} lines of code)."
            )
        )
    }

    if (pkg_summ$num_src_fns > 0) {
        langs <- vapply (strsplit (pkg_summ$languages, ":"), function (i) {
            i [1]
        }, character (1))
        langs <- paste0 (langs [langs != "R"], collapse = ", ")
        s <- c (
            s,
            paste0 (
                "{pkg_summ$num_src_fns} {langs} functions ",
                "(median {pkg_summ$loc_src_fns} lines of code)."
            )
        )
    }

    s <- c (
        s,
        "{pkg_summ$num_params_per_fn} parameters per function (median)."
    )


    cli::cli_li (s)
}

#' Print dependency information
#' @noRd
print_deps <- function (checks) {

    deps <- knitr::kable (
        pkgdeps_as_table (checks),
        row.names = FALSE
    )

    fns_raw <- checks$pkg$external_fns

    fns <- lapply (seq_along (fns_raw), function (i) {
        tallies <- paste0 (
            names (fns_raw [[i]]),
            " (", fns_raw [[i]], ")"
        )
    })
    names (fns) <- names (fns_raw)

    cli::cli_h2 ("Package Dependency Usage")

    cli::cli_alert_info (
        "Package depends on the following packages:"
    )
    cli::cli_text (
        "  (where 'ncalls' is numbers of calls made to each package)"
    )
    message ("")
    print (deps)
    message ("")

    cli::cli_alert_info (
        "Tallies of numbers of function calls to each dependent package:"
    )
    for (f in seq_along (fns)) {
        cli::cli_h3 (names (fns) [f])
        cli::cli_text (paste0 (fns [[f]], collapse = ", "))
    }
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
    if (!output_fn %in% pkg_fns) {
        return (NULL)
    }

    chk_output <- do.call (output_fn, list (checks), envir = pkg_env)

    return (chk_output$print)
}

print_check_screen <- function (checks, what, pkg_env) {

    if (length (checks$checks [[what]]) == 0L) {
        return ()
    }

    output_fn <- get (paste0 ("output_pkgchk_", what), envir = pkg_env)

    chk_output <- do.call (output_fn, list (checks), envir = pkg_env)

    has_print <- all (nzchar (chk_output$print)) | length (chk_output$print) > 1L
    if (!has_print) {
        return ()
    }

    if (chk_output$check_pass) {
        cli::cli_alert_success (chk_output$print$msg_pre)
    } else {
        cli::cli_alert_danger (chk_output$print$msg_pre)
    }

    if (is.vector (chk_output$print$obj)) {
        cli::cli_ul ()
        cli::cli_li (chk_output$print$obj)
        cli::cli_end ()
    }

    if (length (chk_output$print$msg_post) > 0L) {
        cli::cli_text ()
        cli::cli_text (chk_output$print$msg_post)
    }
}

print_check_md <- function (checks, what, pkg_env) {

    output_fn <- get (paste0 ("output_pkgchk_", what), envir = pkg_env)

    chk_output <- do.call (output_fn, list (checks), envir = pkg_env)

    out <- NULL

    has_print <- all (nzchar (chk_output$print)) | length (chk_output$print) > 1L
    if (!has_print) {
        return (out)
    }

    if (!chk_output$check_pass) {
        out <- c (
            "",
            paste0 (symbol_crs (), " ", chk_output$print$msg_pre)
        )
    } else if (nchar (chk_output$print$msg_pre) > 0L) {
        out <- c (
            "",
            paste0 (symbol_tck (), " ", chk_output$print$msg_pre)
        )
    }

    if (!is.null (out) & is.vector (chk_output$print$obj)) {
        out <- c (
            out,
            "",
            paste0 ("- ", chk_output$print$obj),
            ""
        )
    }

    if (length (chk_output$print$msg_post) > 0L) {
        out <- c (
            out,
            "",
            chk_output$print$msg_post,
            ""
        )
    }

    return (out)
}

extra_check_prints_from_env <- function (checks) {

    extra_env <- options ("pkgcheck_extra_env") [[1]]
    if (!is.list (extra_env)) {
        extra_env <- list (extra_env)
    }

    extra_prints <- lapply (extra_env, function (e) {
        e <- env2namespace (e)
        output_fns <- grep ("^output\\_pkgchk\\_", ls (e), value = TRUE)
        output_fns <- gsub ("^output\\_pkgchk\\_", "", output_fns)
        output_fns [which (output_fns %in% names (checks$checks))]
    })

    lens <- vapply (extra_prints, length, integer (1))
    extra_prints <- extra_prints [which (lens > 0L)]
    extra_env <- extra_env [which (lens > 0L)]

    return (list (
        env = extra_env,
        prints = extra_prints
    ))
}

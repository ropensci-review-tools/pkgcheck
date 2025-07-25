# IMPORTANT: All sub-functions with `summarise_` prefixes summarise the actual
# checks, and include a return value specifying either "tick or cross", or just
# "cross only." The latter denotes checks which only appear when they fail,
# while the former appear in the summary list of green ticks required for a
# package to pass all checks.
#
# Any additional checks added must also specify `@return` values as either "tick
# or cross" (important checks which must be pased) or "cross only" (less
# important checks which only appear when failed).

#' Summarise main checklist items for editor report
#' @param checks Result of main \link{pkgcheck} function
#' @noRd
summarise_all_checks <- function (checks) {

    pkg_env <- asNamespace ("pkgcheck")
    pkg_fns <- ls (pkg_env)

    output_fns <- gsub (
        "^output\\_pkgchk\\_", "",
        grep ("^output\\_pkgchk\\_", pkg_fns, value = TRUE)
    )

    has_covr <- "covr" %in% names (checks$goodpractice)
    if (!has_covr) {
        output_fns <- output_fns [which (!grepl ("covr", output_fns))]
    }
    ordered_checks <- order_checks (output_fns)
    out <- lapply (
        ordered_checks,
        function (i) summarise_check (checks, i, pkg_env)
    )

    # "watch" checks; issue #144, #248
    watch_index <- vapply (out, function (i) {
        ret <- c (FALSE, FALSE)
        if (!is.null (i) && "check_type" %in% names (attributes (i))) {
            a <- attr (i, "check_type")
            ret <- c (grepl ("^watch", a), grepl ("\\_watch$", a))
        }
        return (ret)
    }, logical (2L))
    index_tick <- which (watch_index [1, ])
    out [index_tick] <-
        gsub ("\\:heavy\\_check\\_mark:", ":eyes:", out [index_tick])
    index_cross <- which (watch_index [2, ])
    out [index_cross] <-
        gsub ("\\:heavy\\_multiplication\\_x\\:", ":eyes:", out [index_cross])

    out <- do.call (c, out)
    # Ensure all :eyes: come last:
    index_eyes <- grep ("\\:eyes\\:", out)
    index_other <- seq_along (out)
    if (length (index_eyes) > 0L) {
        index_other <- index_other [-index_eyes]
    }
    out <- c (out [index_other], out [index_eyes])


    out <- c (out, summarise_extra_env_checks (checks))

    gp <- summarise_gp_checks (checks)
    if (any (grepl ("^rcmd\\_", names (gp)))) {
        out <- c (
            out,
            gp$rcmd_errs,
            gp$rcmd_warns
        )
    }

    # re-order "watch" checks to bottom
    index1 <- grep ("\\:heavy\\_(multiplication\\_x|check\\_mark)\\:", out)
    index2 <- grep ("\\:eyes\\:", out)
    out <- out [c (index1, index2)]

    checks_okay <- !any (grepl (symbol_crs (), out))
    if (!checks_okay) {
        out <- c (
            out,
            "",
            paste0 (
                "**Important:** All failing checks above ",
                "must be addressed prior to proceeding"
            )
        )
    }

    if (any (grepl ("\\:eyes\\:", out))) {
        out <- c (
            out,
            "",
            "(Checks marked with :eyes: may be optionally addressed.)",
            ""
        )
    }

    attr (out, "checks_okay") <- checks_okay

    return (out)
}

summarise_extra_env_checks <- function (checks) {

    extra_env <- options ("pkgcheck_extra_env") [[1]]
    if (is.null (extra_env)) {
        return (NULL)
    }
    if (!is.list (extra_env)) {
        extra_env <- list (extra_env)
    }

    extra_chks <- lapply (extra_env, function (e) {
        e <- env2namespace (e)
        output_fns <- grep ("^output\\_pkgchk\\_", ls (e), value = TRUE)
        output_fns <- gsub ("^output\\_pkgchk\\_", "", output_fns)
        output_fns <- output_fns [which (output_fns %in% names (checks$checks))]
        vapply (output_fns,
            function (i) summarise_check (checks, i, e),
            character (1),
            USE.NAMES = FALSE
        )
    })

    return (unlist (extra_chks))
}

#' Function to specify the order in which checks appear in the summary method.
#'
#' @param fns List of output functions with prefixes `output_pkgchk_`, for which
#' order is to be established.
#' @return Modified version of input list with functions ordered in specified
#' sequence.
#' @noRd
order_checks <- function (fns) {

    ord <- c (
        "pkgname",
        "license",
        "has_citation",
        "has_codemeta",
        "has_contrib",
        "fns_have_return_vals",
        "uses_roxygen2",
        "pkgdown",
        "has_url",
        "has_bugs",
        "has_vignette",
        "fns_have_exs",
        "global_assign",
        "ci",
        "covr",
        "lintr",
        "has_scrap",
        "left_assign",
        "renv_activated",
        "branch_is_master",
        "srr_okay",
        "srr_missing",
        "srr_todo",
        "srr_most_in_one_file",
        # These are "watch" checks, not outright fails; they must be
        # additionally explicitly listed below in `watch_checks()`:
        "obsolete_pkg_deps",
        "unique_fn_names",
        "num_imports"
    )

    fns <- fns [which (fns %in% ord)]
    ord <- ord [which (ord %in% fns)] # b/c 'covr' is removed w/o gp
    fns <- fns [match (ord, fns)]

    return (fns)
}

#' Generic function to summarise checks based on result of corresponding
#' `output_pkgchk_` function.
#'
#' @param checks Full result of `pkgcheck()` call
#' @param what Name of check which must also correspond to an internal function
#' named `output_pkgchk_<name>`.
#' @param pkg_env A namespace environment generated by `env2namespace`.
#' @return Check formatted to apepar in `summary` method
#' @noRd
summarise_check <- function (checks, what, pkg_env) {

    pkg_fns <- ls (pkg_env)
    summary_fn <- paste0 ("output_pkgchk_", what)

    if (!summary_fn %in% pkg_fns) {
        return (NULL)
    }

    chk_summary <- do.call (summary_fn, list (checks), envir = pkg_env)

    res <- NULL

    if (sum (nchar (chk_summary$summary)) > 0L) {
        res <- paste0 (
            "- ",
            ifelse (chk_summary$check_pass,
                symbol_tck (),
                symbol_crs ()
            ),
            " ",
            chk_summary$summary
        )
    }

    if (!is.null (res) && "check_type" %in% names (chk_summary)) {
        attr (res, "check_type") <- chk_summary$check_type
    }

    return (res)
}

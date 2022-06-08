
#' Check whether the package uses renv
#'
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @return TRUE if `renv` is used; otherwise FALSE
#' @noRd
pkgchk_renv_activated <- function (checks) {

    return (checks$info$renv_activated)
}

output_pkgchk_renv_activated <- function (checks) {

    out <- list (
        check_pass = !checks$checks$renv_activated,
        summary = "",
        print = ""
    )

    if (!out$check_pass) {
        out$summary <- "Package has renv activated"
    }

    return (out)
}

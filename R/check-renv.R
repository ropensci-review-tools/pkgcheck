
#' Check whether the package uses renv
#'
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @return TRUE if `renv` is used; otherwise FALSE
#' @noRd
pkgchk_uses_renv <- function (checks) {

    return (checks$info$uses_renv)
}

output_pkgchk_uses_renv <- function (checks) {

    out <- list (
        check_pass = !checks$checks$uses_renv,
        summary = "",
        print = ""
    )

    if (!out$check_pass) {
        out$summary <- "Package uses renv"
    }

    return (out)
}

#' Check for failed lintr checks in the package.
#'
#' This function runs all available `lintr_*` checks from the \pkg{goodpractice} package
#' on the target package and returns the names of any failed checks.
#'
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @return Character vector of failed `lintr_*` check names; returns `character(0)`
#' if all lintr checks pass.
#' @noRd
pkgchk_lintr <- function (checks) {

    failed_checks <- goodpractice::failed_checks (checks$goodpractice)
    grep ("^lintr", failed_checks, value = TRUE)

}

output_pkgchk_lintr <- function (checks) {

    failed <- checks$checks$lintr

    out <- list (
        check_pass = length (failed) == 0L,
        summary = "",
        print = ""
    ) # no print method


    if (out$check_pass) {
        out$summary <- "All goodpractice linters passed."
    } else {
        out$summary <- "Some goodpractice linters failed."
    }

    return (out)
}

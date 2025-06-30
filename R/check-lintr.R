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

    lintr_checks <- grep ("^lintr_", goodpractice::all_checks (), value = TRUE)
    lintr_results <- goodpractice::gp (checks$pkg$path, checks = lintr_checks)

    goodpractice::failed_checks (lintr_results)
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
        out$summary <- paste0 (
            "These goodpractice linters failed: [",
            paste (failed, collapse = ", "),
            "]."
        )
    }

    return (out)
}

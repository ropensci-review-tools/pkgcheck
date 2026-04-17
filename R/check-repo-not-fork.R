#' Check that GitHub repository is not a fork.
#'
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @return `TRUE` if repository is a fork, otherwise `FALSE`.
#' @noRd
pkgchk_repo_not_fork <- function (checks) {

    checks$info$github$repo_not_fork
}

output_pkgchk_repo_not_fork <- function (checks) {

    out <- list (
        check_pass = checks$checks$repo_not_fork,
        summary = "",
        print = ""
    )

    if (!out$check_pass) {
        out$summary <- "Repository is a fork, not an original source repository"
    }

    return (out)
}

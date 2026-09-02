#' Check that GitHub repository lists an associated website
#'
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @return `TRUE` if repository has a website, otherwise `FALSE`.
#' @noRd
pkgchk_repo_has_website <- function (checks) {

    checks$info$github$repo_has_website
}

output_pkgchk_repo_has_website <- function (checks) {

    out <- list (
        check_pass = checks$checks$repo_has_website,
        summary = "",
        print = ""
    )

    if (out$check_pass) {
        out$summary <- "Repository has a website"
    } else {
        out$summary <- "Repository has no website"
    }

    return (out)
}

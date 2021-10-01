#' Check whether a package has a `inst/CITATION` file
#'
#' This does not check the contents of that file in any way.
#'
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @noRd
pkgchk_has_citation <- function (checks) {
    "CITATION" %in% list.files (file.path (checks$package$path, "inst"))
}

output_pkgchk_has_citation <- function (checks) {
    out <- list (
        check_pass = checks$checks$has_citation,
        summary = "",
        print = ""
    ) # no print method

    out$summary <- paste0 (
        ifelse (out$check_pass, "has", "does not have"),
        " a 'CITATION' file."
    )

    return (out)
}


#' Check whether a package has a `codemeta.json` file.
#'
#' "codemeta.json" files are recommended for all rOpenSci packages, as
#' documented [in our "*Packaging
#' Guide*](https://devguide.ropensci.org/pkg_building.html#creating-metadata-for-your-package).
#'
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @noRd
pkgchk_has_codemeta <- function (checks) {

    "codemeta.json" %in% list.files (checks$pkg$path, recursive = FALSE)
}

output_pkgchk_has_codemeta <- function (checks) {

    out <- list (
        check_pass = checks$checks$has_codemeta,
        summary = "",
        print = ""
    ) # no print method

    out$summary <- paste0 (
        ifelse (out$check_pass, "has", "does not have"),
        " a 'codemeta.json' file."
    )

    return (out)
}

# https://github.com/yihui/knitr-examples/blob/master/113-externalization.Rmd

# ---- pkgchk-citation ----
#' Check whether a package has a `inst/CITATION` file.
#'
#' "CITATION" files are required for all rOpenSci packages, as documented [in
#' our "*Packaging
#' Guide*](https://devguide.ropensci.org/pkg_building.html#citation-file). This
#' does not check the contents of that file in any way.
#'
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @noRd
pkgchk_has_citation <- function (checks) {

    "CITATION" %in% list.files (fs::path (checks$pkg$path, "inst"))
}

# ---- output-pkgchk-citation ----
output_pkgchk_has_citation <- function (checks) {

    out <- list (
        check_pass = checks$checks$has_citation,
        summary = "",
        print = ""
    )

    # disabled:
    # https://github.com/ropensci-review-tools/pkgcheck/issues/115
    # out$summary <- paste0 (
    #    ifelse (out$check_pass, "has", "does not have"),
    #    " a 'CITATION' file."
    # )

    return (out)
}

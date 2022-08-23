
#' Check whether the package license is acceptable.
#'
#' Details of acceptable licenses are provided in the links in [our *Packaging
#' Guide*](https://devguide.ropensci.org/building.html#licence).
#'
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @return Character vector of any unacceptable license entries; otherwise
#' a character(0) if license(s) is/are acceptable.
#' @noRd
pkgchk_license <- function (checks) {

    # https://cran.r-project.org/doc/manuals/R-exts.html#Licensing
    # multiple licenses must be separated by vertical bars
    licenses <- strsplit (checks$pkg$license, "\\|") [[1]]

    llist <- paste0 (license_list (), collapse = "|")
    okay <- vapply (
        licenses, function (i) {
            grepl (llist, i)
        },
        logical (1)
    )

    names (okay [which (!okay)])
}

output_pkgchk_license <- function (checks) {

    out <- list (
        check_pass = length (checks$checks$license) == 0L,
        summary = "",
        print = ""
    )

    if (!out$check_pass) {
        out$summary <- paste0 (
            "Package contains unacceptable 'License' entries: [",
            checks$checks$license, "]"
        )
    }

    return (out)
}

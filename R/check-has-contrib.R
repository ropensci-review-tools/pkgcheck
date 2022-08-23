

#' Check whether package has contributor guidelines in a 'contributing' file.
#'
#' A "contributing" file is required for all rOpenSci packages, as documented
#' [in our "*Contributing
#' Guide*](https://devguide.ropensci.org/collaboration.html?q=contributing#contributing-guide).
#'
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @return Logical flag
#' @noRd
pkgchk_has_contrib_md <- function (checks) {

    flist <- list.files (
        checks$pkg$path,
        all.files = TRUE,
        recursive = TRUE,
        full.names = FALSE
    )

    # remove any renv files (#141). The 'pattern' param of
    # flist only works as positive filter.
    ptn <- paste0 (.Platform$file.sep, "renv", .Platform$file.sep)
    flist <- flist [which (!grepl (ptn, flist))]

    flist <- vapply (flist, function (i) {
        utils::tail (decompose_path (i) [[1]], 1L)
    },
    character (1),
    USE.NAMES = FALSE
    )

    chk <- any (grepl ("^contributing(\\.|$)", flist, ignore.case = TRUE))

    return (chk)
}

output_pkgchk_has_contrib <- function (checks) {

    out <- list (
        check_pass = checks$checks$has_contrib,
        summary = "",
        print = ""
    ) # no print method

    out$summary <- paste0 (
        ifelse (out$check_pass, "has", "does not have"),
        " a 'contributing' file."
    )

    return (out)
}
